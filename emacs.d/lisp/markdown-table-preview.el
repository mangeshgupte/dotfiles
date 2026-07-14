;;; markdown-table-preview.el --- Inline SVG previews of markdown tables -*- lexical-binding: t; -*-

;; Author: Mangesh
;; Description: Render GitHub-flavored markdown pipe tables to inline SVG
;;              images -- generated in pure Emacs Lisp, no external tools --
;;              and display them as overlays directly over the table source,
;;              LaTeX-preview style.  Wide columns wrap within a fixed width
;;              so columns stay aligned even when a cell overflows.
;;
;; Requirements:
;;   - Emacs built with SVG support (image-type-available-p 'svg)
;;
;; Behavior:
;;   - Each pipe table is parsed and laid out on a monospace grid, so
;;     columns align regardless of the buffer's (variable-pitch) font.
;;   - A column wider than `markdown-table-preview-max-column-chars' wraps
;;     to that width; narrow columns keep their natural width.
;;   - Previews fit the window: when a table would be wider than the
;;     narrowest window showing the buffer, wide columns wrap tighter
;;     (never below `markdown-table-preview-min-column-chars'), and a
;;     table that still cannot fit is scaled down so nothing is clipped.
;;     Previews re-render when the window width changes.
;;   - The rendered SVG replaces the table source via an overlay.  Move
;;     point into a table to reveal the source for editing; move out and
;;     the image returns.  Save to re-render.  Toggling the mode off
;;     removes every overlay.
;;   - Lines inside fenced code blocks are skipped.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'markdown-emoji)

(defgroup markdown-table-preview nil
  "Inline SVG previews of markdown tables."
  :group 'markdown)

(defcustom markdown-table-preview-font-size 15
  "SVG font size in pixels for rendered tables."
  :type 'integer :group 'markdown-table-preview)

(defcustom markdown-table-preview-font-family "Menlo, monospace"
  "Monospace font family used in rendered tables.
Must be monospace: column widths assume a fixed character advance."
  :type 'string :group 'markdown-table-preview)

(defcustom markdown-table-preview-char-width-ratio 0.6
  "Advance width of one character as a fraction of the font size.
0.6 matches Menlo and most monospace fonts."
  :type 'number :group 'markdown-table-preview)

(defcustom markdown-table-preview-max-column-chars 44
  "Columns wider than this many characters wrap to this width."
  :type 'integer :group 'markdown-table-preview)

(defcustom markdown-table-preview-fit-window t
  "If non-nil, fit table previews to the window width.
Wide columns wrap more aggressively (down to
`markdown-table-preview-min-column-chars') until the table fits the
narrowest window displaying the buffer; a table that still cannot
fit is scaled down.  Previews re-render when the window width
changes."
  :type 'boolean :group 'markdown-table-preview)

(defcustom markdown-table-preview-min-column-chars 8
  "Narrowest wrap width that window fitting may impose on a column."
  :type 'integer :group 'markdown-table-preview)

(defcustom markdown-table-preview-scale 1.0
  "Scale factor applied to rendered SVG images."
  :type 'number :group 'markdown-table-preview)

(defcustom markdown-table-preview-render-on-save t
  "If non-nil, re-render previews after each save."
  :type 'boolean :group 'markdown-table-preview)

;; Defined by `define-minor-mode' at the bottom; declared here for the
;; helper functions above it that check the mode state.
(defvar markdown-table-preview-mode)

(defvar-local markdown-table-preview--overlays nil
  "List of preview overlays in the current buffer.")

(defvar-local markdown-table-preview--last-width nil
  "Pixel width the previews were last rendered for, or nil.")

(defvar-local markdown-table-preview--resize-timer nil
  "Pending idle timer for a fit re-render, or nil.")

;;; Colors -------------------------------------------------------------

(defun markdown-table-preview--colors ()
  "Return a plist of colors matching the current frame background mode."
  (if (eq (frame-parameter nil 'background-mode) 'dark)
      (list :bg "#1e1e1e" :header "#2d2d2d" :grid "#555555" :fg "#dcdcdc")
    (list :bg "#ffffff" :header "#f0f0f0" :grid "#b8b8b8" :fg "#222222")))

;;; Parsing ------------------------------------------------------------

(defun markdown-table-preview--separator-line-p (line)
  "Return non-nil if LINE is a markdown table separator row."
  (let ((s (string-trim line)))
    (and (string-match-p "-" s)
         (not (string-match-p "[^-:| \t]" s)))))

(defun markdown-table-preview--split-row (line)
  "Split a pipe-table LINE into a list of trimmed cell strings."
  (let ((s (string-trim line)))
    (setq s (replace-regexp-in-string "\\`|" "" s))
    (setq s (replace-regexp-in-string "|\\'" "" s))
    (mapcar #'string-trim (split-string s "|"))))

(defun markdown-table-preview--cell-align (cell)
  "Return the alignment symbol (`left', `right', or `center') for CELL."
  (let ((s (string-trim cell)))
    (cond ((and (string-prefix-p ":" s) (string-suffix-p ":" s)) 'center)
          ((string-suffix-p ":" s) 'right)
          (t 'left))))

(defun markdown-table-preview--pad (row n)
  "Pad with empty cells or truncate ROW to exactly N cells."
  (let ((len (length row)))
    (cond ((= len n) row)
          ((< len n) (append row (make-list (- n len) "")))
          (t (cl-subseq row 0 n)))))

(defun markdown-table-preview--line-at-point ()
  "Return the current line's text without properties."
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

(defun markdown-table-preview--fence-line-p (line)
  "Return non-nil if LINE opens or closes a fenced code block."
  (string-match-p "\\`[ \t]*\\(```\\|~~~\\)" line))

(defun markdown-table-preview--tables ()
  "Scan the buffer and return a list of tables.
Each element is (BEG END ROWS ALIGNS): BEG/END delimit the table
text, ROWS is a list of cell-string lists (header first, separator
dropped, each padded to the column count), and ALIGNS is a
per-column alignment symbol list.  Fenced code blocks are skipped."
  (let (tables (in-fence nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (markdown-table-preview--line-at-point)))
          (cond
           ((markdown-table-preview--fence-line-p line)
            (setq in-fence (not in-fence))
            (forward-line 1))
           (in-fence (forward-line 1))
           ;; A header line immediately followed by a separator starts a table.
           ((and (string-match-p "|" line)
                 (save-excursion
                   (forward-line 1)
                   (and (not (eobp))
                        (markdown-table-preview--separator-line-p
                         (markdown-table-preview--line-at-point)))))
            (let ((beg (line-beginning-position))
                  (header (markdown-table-preview--split-row line))
                  aligns rows end)
              (forward-line 1)          ; onto the separator row
              (setq aligns (mapcar #'markdown-table-preview--cell-align
                                   (markdown-table-preview--split-row
                                    (markdown-table-preview--line-at-point))))
              (setq end (line-end-position))
              (forward-line 1)          ; onto the first body row
              (let ((body '()))
                (while (and (not (eobp))
                            (let ((l (markdown-table-preview--line-at-point)))
                              (and (string-match-p "|" l)
                                   (not (string-blank-p l))
                                   (not (markdown-table-preview--fence-line-p l)))))
                  (push (markdown-table-preview--split-row
                         (markdown-table-preview--line-at-point))
                        body)
                  (setq end (line-end-position))
                  (forward-line 1))
                (setq rows (cons header (nreverse body))))
              (let ((ncols (length aligns)))
                (setq rows (mapcar (lambda (r) (markdown-table-preview--pad r ncols))
                                   rows)))
              (push (list beg end rows aligns) tables)))
           (t (forward-line 1))))))
    (nreverse tables)))

;;; Text wrapping ------------------------------------------------------

(defun markdown-table-preview--wrap (text width)
  "Word-wrap TEXT into a list of lines of at most WIDTH characters.
Tokens longer than WIDTH are hard-broken."
  (setq text (string-trim text))
  (if (<= (length text) width)
      (list text)
    (let ((words (split-string text)) (lines '()) (cur ""))
      (dolist (w words)
        (while (> (length w) width)
          (when (> (length cur) 0) (push cur lines) (setq cur ""))
          (push (substring w 0 width) lines)
          (setq w (substring w width)))
        (cond
         ((string-empty-p cur) (setq cur w))
         ((<= (+ (length cur) 1 (length w)) width)
          (setq cur (concat cur " " w)))
         (t (push cur lines) (setq cur w))))
      (when (> (length cur) 0) (push cur lines))
      (nreverse lines))))

;;; Inline styling -----------------------------------------------------

(defun markdown-table-preview--parse-inline (text)
  "Strip **bold**/__bold__ markers from TEXT.
Return (VISIBLE . BOLD): VISIBLE is TEXT with the markers removed,
trimmed, and inner whitespace runs collapsed to single spaces;
BOLD is a bool-vector marking VISIBLE's bold characters.  Column
widths and wrapping must use VISIBLE so the markers take no space."
  (let ((chars '()) (flags '()) (pos 0))
    ;; Like GFM, the delimited text may not start or end with whitespace.
    (while (string-match
            "\\(\\*\\*\\|__\\)\\([^ \t]\\(?:.*?[^ \t]\\)?\\)\\1" text pos)
      (let ((inner (match-string 2 text)))
        (dotimes (i (- (match-beginning 0) pos))
          (push (aref text (+ pos i)) chars) (push nil flags))
        (dotimes (i (length inner))
          (push (aref inner i) chars) (push t flags))
        (setq pos (match-end 0))))
    (dotimes (i (- (length text) pos))
      (push (aref text (+ pos i)) chars) (push nil flags))
    ;; Trim and collapse whitespace, keeping the flags aligned: a
    ;; collapsed space keeps its own flag so a bold phrase stays one run.
    (let ((out '()) (oflags '()) (pending nil) (pending-flag nil))
      (cl-loop for ch in (nreverse chars) for f in (nreverse flags) do
               (if (memq ch '(?\s ?\t))
                   (when out (setq pending t pending-flag f))
                 (when pending (push ?\s out) (push pending-flag oflags)
                       (setq pending nil))
                 (push ch out) (push f oflags)))
      (let* ((s (apply #'string (nreverse out)))
             (bv (make-bool-vector (length s) nil)))
        (cl-loop for f in (nreverse oflags) for i from 0
                 do (when f (aset bv i t)))
        (cons s bv)))))

(defun markdown-table-preview--line-runs (visible bold width)
  "Wrap VISIBLE to WIDTH and split each line into style runs.
BOLD is VISIBLE's bold mask from `markdown-table-preview--parse-inline'.
Return a list of lines, each a list of (TEXT . BOLD-P) runs; a blank
line is the empty list."
  (let ((lines (markdown-table-preview--wrap visible width))
        (pos 0) out)
    (dolist (ln lines)
      ;; A break at a word boundary drops one space from VISIBLE; a
      ;; hard break inside a long word drops nothing.
      (when (and (< pos (length visible)) (eq (aref visible pos) ?\s))
        (setq pos (1+ pos)))
      (let ((runs '()) (i 0) (n (length ln)))
        (while (< i n)
          (let ((b (aref bold (+ pos i))) (j i))
            (while (and (< j n) (eq (aref bold (+ pos j)) b))
              (setq j (1+ j)))
            (push (cons (substring ln i j) b) runs)
            (setq i j)))
        (push (nreverse runs) out)
        (setq pos (+ pos n))))
    (nreverse out)))

;;; Window fitting -----------------------------------------------------

(defun markdown-table-preview--fit-cap (natural cw padx target-px)
  "Return the column wrap cap, in characters, that fits TARGET-PX.
NATURAL is the vector of uncapped column widths, CW the pixel
advance per character, PADX the horizontal cell padding.  Returns
the largest cap no greater than
`markdown-table-preview-max-column-chars' whose total table width
fits TARGET-PX (total width is monotonic in the cap, so binary
search), but never less than
`markdown-table-preview-min-column-chars' -- the caller scales the
image down when even that is too wide."
  (let* ((ncols (length natural))
         (maxc markdown-table-preview-max-column-chars)
         (minc (max 1 (min markdown-table-preview-min-column-chars maxc)))
         (width-at (lambda (cap)
                     (let ((tw 0))
                       (dotimes (c ncols)
                         (setq tw (+ tw (* (min (aref natural c) cap) cw)
                                     (* 2 padx))))
                       tw))))
    (if (<= (funcall width-at maxc) target-px)
        maxc
      (let ((lo minc) (hi maxc))
        (while (< lo hi)
          (let ((mid (/ (+ lo hi 1) 2)))
            (if (<= (funcall width-at mid) target-px)
                (setq lo mid)
              (setq hi (1- mid)))))
        lo))))

(defun markdown-table-preview--available-width ()
  "Return the usable pixel width for previews, or nil when unconstrained.
This is the text-area width of the narrowest window displaying the
buffer (window margins, e.g. olivetti's, are already excluded),
less a small edge gap.  Returns nil when fitting is disabled or the
buffer is not displayed in any window."
  (when markdown-table-preview-fit-window
    (let ((windows (get-buffer-window-list nil nil t)))
      (when windows
        (max 1 (- (apply #'min (mapcar (lambda (w) (window-body-width w t))
                                       windows))
                  (* 2 (frame-char-width))))))))

;;; SVG generation -----------------------------------------------------

(defun markdown-table-preview--xml-escape (s)
  "XML-escape the ampersands and angle brackets in S."
  (setq s (replace-regexp-in-string "&" "&amp;" s t t))
  (setq s (replace-regexp-in-string "<" "&lt;" s t t))
  (setq s (replace-regexp-in-string ">" "&gt;" s t t))
  s)

(defun markdown-table-preview--runs-xml (runs)
  "Return the XML text content for RUNS, a list of (TEXT . BOLD-P).
Bold runs become <tspan> continuations, which inherit the current
text position, so mixed-style lines stay on the monospace grid."
  (mapconcat (lambda (run)
               (let ((esc (markdown-table-preview--xml-escape (car run))))
                 (if (cdr run)
                     (concat "<tspan font-weight=\"bold\">" esc "</tspan>")
                   esc)))
             runs ""))

(defun markdown-table-preview--svg (rows aligns &optional target-px)
  "Return an SVG string rendering ROWS with per-column ALIGNS.
ROWS is a list of cell-string lists (header first).  When TARGET-PX
is non-nil, wrap columns tighter as needed so the table fits that
pixel width, within the limits of
`markdown-table-preview-min-column-chars'."
  (let* ((ncols (length aligns))
         (fs markdown-table-preview-font-size)
         (cw (max 1 (round (* fs markdown-table-preview-char-width-ratio))))
         (lh (round (* fs 1.4)))
         (padx cw)
         (pady (max 2 (round (* fs 0.3))))
         (maxc markdown-table-preview-max-column-chars)
         (colors (markdown-table-preview--colors))
         (widths (make-vector ncols 1))
         (nrows (length rows))
         wrapped colpx xpos rowh ypos totalw totalh parts)
    ;; Emoji would abort Emacs when librsvg lays out the SVG <text> (its
    ;; Pango/CoreText backend crashes on color-font fallback), so replace
    ;; them before any width math -- see `markdown-emoji-sanitize'.
    (setq rows (mapcar (lambda (r) (mapcar #'markdown-emoji-sanitize r)) rows))
    ;; Strip **bold** markers, keeping a per-character bold mask: each
    ;; cell becomes (VISIBLE . BOLD), and all width math below uses the
    ;; visible text only.
    (setq rows (mapcar (lambda (r)
                         (mapcar #'markdown-table-preview--parse-inline r))
                       rows))
    ;; Natural (single-line) width per column, then the cap: MAXC,
    ;; tightened further when the table must fit TARGET-PX.
    (dotimes (c ncols)
      (let ((mx 1))
        (dolist (r rows)
          (setq mx (max mx (length (car (nth c r))))))
        (aset widths c mx)))
    (let ((cap (if target-px
                   (markdown-table-preview--fit-cap widths cw padx target-px)
                 maxc)))
      (dotimes (c ncols)
        (aset widths c (min (aref widths c) cap))))
    ;; Wrap every cell to its column width, splitting lines into runs.
    (setq wrapped
          (mapcar (lambda (r)
                    (let ((cells '()))
                      (dotimes (c ncols)
                        (let ((cell (nth c r)))
                          (push (markdown-table-preview--line-runs
                                 (car cell) (cdr cell) (aref widths c))
                                cells)))
                      (nreverse cells)))
                  rows))
    ;; Column pixel widths and x offsets.
    (setq colpx (make-vector ncols 0))
    (dotimes (c ncols) (aset colpx c (+ (* (aref widths c) cw) (* 2 padx))))
    (setq xpos (make-vector (1+ ncols) 0))
    (dotimes (c ncols) (aset xpos (1+ c) (+ (aref xpos c) (aref colpx c))))
    (setq totalw (aref xpos ncols))
    ;; Row pixel heights and y offsets.
    (setq rowh (make-vector nrows 0))
    (let ((ri 0))
      (dolist (r wrapped)
        (let ((maxlines 1))
          (dolist (cell r) (setq maxlines (max maxlines (length cell))))
          (aset rowh ri (+ (* maxlines lh) (* 2 pady))))
        (setq ri (1+ ri))))
    (setq ypos (make-vector (1+ nrows) 0))
    (dotimes (r nrows) (aset ypos (1+ r) (+ (aref ypos r) (aref rowh r))))
    (setq totalh (aref ypos nrows))
    ;; --- Emit SVG ---
    (push (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">"
                  totalw totalh totalw totalh) parts)
    (push (format "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" fill=\"%s\"/>"
                  totalw totalh (plist-get colors :bg)) parts)
    (push (format "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" fill=\"%s\"/>"
                  totalw (aref rowh 0) (plist-get colors :header)) parts)
    (let ((grid (plist-get colors :grid)))
      (dotimes (c (1+ ncols))
        (push (format "<line x1=\"%d\" y1=\"0\" x2=\"%d\" y2=\"%d\" stroke=\"%s\" stroke-width=\"1\"/>"
                      (aref xpos c) (aref xpos c) totalh grid) parts))
      (dotimes (r (1+ nrows))
        (push (format "<line x1=\"0\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"%s\" stroke-width=\"1\"/>"
                      (aref ypos r) totalw (aref ypos r) grid) parts)))
    (let ((fg (plist-get colors :fg)) (ri 0))
      (dolist (r wrapped)
        (let ((header (= ri 0)))
          (dotimes (c ncols)
            (let ((lines (nth c r))
                  (align (nth c aligns))
                  (li 0))
              (dolist (runs lines)
                (when runs
                  (let ((ty (+ (aref ypos ri) pady (* li lh) (round (* fs 0.82))))
                        (anchor (pcase align ('right "end") ('center "middle") (_ "start")))
                        (tx (pcase align
                              ('right (- (aref xpos (1+ c)) padx))
                              ('center (/ (+ (aref xpos c) (aref xpos (1+ c))) 2))
                              (_ (+ (aref xpos c) padx)))))
                    (push (format "<text x=\"%d\" y=\"%d\" font-family=\"%s\" font-size=\"%d\" fill=\"%s\" text-anchor=\"%s\"%s>%s</text>"
                                  tx ty markdown-table-preview-font-family fs fg anchor
                                  (if header " font-weight=\"bold\"" "")
                                  (markdown-table-preview--runs-xml runs))
                          parts)))
                (setq li (1+ li))))))
        (setq ri (1+ ri))))
    (push "</svg>" parts)
    (mapconcat #'identity (nreverse parts) "\n")))

(defun markdown-table-preview--image (rows aligns &optional max-px)
  "Return an SVG image object for ROWS/ALIGNS.
When MAX-PX is non-nil, reflow the table to fit that many pixels
and, as a last resort, let the display engine scale the image down
to it (`:max-width'), so the preview is never clipped."
  (let ((layout-px (and max-px
                        (max 1 (floor (/ max-px
                                         markdown-table-preview-scale))))))
    (apply #'create-image
           (markdown-table-preview--svg rows aligns layout-px)
           'svg t
           :scale markdown-table-preview-scale
           :ascent 'center
           (when max-px (list :max-width max-px)))))

;;; Overlays -----------------------------------------------------------

(defun markdown-table-preview--make-overlay (beg end image)
  "Overlay BEG..END with IMAGE, replacing the table source."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'markdown-table-preview t)
    (overlay-put ov 'mtp-image image)
    (overlay-put ov 'display image)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'help-echo "Markdown table preview -- move point in to edit")
    (push ov markdown-table-preview--overlays)
    ov))

(defun markdown-table-preview--clear ()
  "Remove all preview overlays in the buffer."
  (mapc (lambda (ov) (when (overlayp ov) (delete-overlay ov)))
        markdown-table-preview--overlays)
  (setq markdown-table-preview--overlays nil)
  (remove-overlays (point-min) (point-max) 'markdown-table-preview t))

(defun markdown-table-preview--reveal ()
  "Show source for the table under point, image for the others."
  (dolist (ov markdown-table-preview--overlays)
    (when (overlay-buffer ov)
      (if (and (>= (point) (overlay-start ov))
               (<= (point) (overlay-end ov)))
          (overlay-put ov 'display nil)
        (overlay-put ov 'display (overlay-get ov 'mtp-image))))))

;;; Rendering ----------------------------------------------------------

(defun markdown-table-preview-render-buffer ()
  "Render or refresh SVG previews for every pipe table in the buffer."
  (interactive)
  (markdown-table-preview--clear)
  (let ((target (markdown-table-preview--available-width)))
    (setq markdown-table-preview--last-width target)
    (dolist (tbl (markdown-table-preview--tables))
      (pcase-let ((`(,beg ,end ,rows ,aligns) tbl))
        (condition-case err
            (markdown-table-preview--make-overlay
             beg end (markdown-table-preview--image rows aligns target))
          (error (message "markdown-table-preview: %s"
                          (error-message-string err)))))))
  ;; A table under point stays revealed after a re-render.
  (markdown-table-preview--reveal))

(defun markdown-table-preview--after-save ()
  "Re-render previews on save when enabled."
  (when (and markdown-table-preview-mode markdown-table-preview-render-on-save)
    (markdown-table-preview-render-buffer)))

(defun markdown-table-preview--schedule-render (window)
  "Schedule a fit re-render for WINDOW's buffer.
Added buffer-locally to `window-size-change-functions' and
`window-buffer-change-functions', so WINDOW shows this buffer.
Runs on a short idle timer: this coalesces bursts of resize events
and measures the window after other hooks (e.g. olivetti's margin
updates) have settled."
  (when (windowp window)
    (with-current-buffer (window-buffer window)
      (when (and markdown-table-preview-mode
                 markdown-table-preview-fit-window
                 (not markdown-table-preview--resize-timer))
        (setq markdown-table-preview--resize-timer
              (run-with-idle-timer
               0.15 nil #'markdown-table-preview--render-if-resized
               (current-buffer)))))))

(defun markdown-table-preview--render-if-resized (buf)
  "Re-render previews in BUF if the available width has changed."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq markdown-table-preview--resize-timer nil)
      (when (and markdown-table-preview-mode
                 (not (eql (markdown-table-preview--available-width)
                           markdown-table-preview--last-width)))
        (markdown-table-preview-render-buffer)))))

;;;###autoload
(define-minor-mode markdown-table-preview-mode
  "Toggle inline SVG previews of markdown tables.
When enabled, each pipe table is rendered to an SVG image (in pure
Emacs Lisp, no external tools) and shown over its source.  Move
point into a table to edit the source; move out to see the image."
  :lighter " Tbl"
  :group 'markdown-table-preview
  (if markdown-table-preview-mode
      (progn
        (unless (image-type-available-p 'svg)
          (user-error "markdown-table-preview: this Emacs lacks SVG support"))
        (add-hook 'after-save-hook #'markdown-table-preview--after-save nil t)
        (add-hook 'post-command-hook #'markdown-table-preview--reveal nil t)
        (add-hook 'window-size-change-functions
                  #'markdown-table-preview--schedule-render nil t)
        (add-hook 'window-buffer-change-functions
                  #'markdown-table-preview--schedule-render nil t)
        (markdown-table-preview-render-buffer))
    (remove-hook 'after-save-hook #'markdown-table-preview--after-save t)
    (remove-hook 'post-command-hook #'markdown-table-preview--reveal t)
    (remove-hook 'window-size-change-functions
                 #'markdown-table-preview--schedule-render t)
    (remove-hook 'window-buffer-change-functions
                 #'markdown-table-preview--schedule-render t)
    (when (timerp markdown-table-preview--resize-timer)
      (cancel-timer markdown-table-preview--resize-timer))
    (setq markdown-table-preview--resize-timer nil)
    (markdown-table-preview--clear)))

(provide 'markdown-table-preview)
;;; markdown-table-preview.el ends here
