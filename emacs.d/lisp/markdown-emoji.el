;;; markdown-emoji.el --- Keep color-emoji out of librsvg-rendered SVG text -*- lexical-binding: t; -*-

;; Author: Mangesh
;; Description: Replace emoji with a neutral placeholder in strings that are
;;   about to be embedded as SVG <text> and rendered by Emacs's librsvg.
;;
;;   On macOS, librsvg lays text out through Pango's CoreText backend, which
;;   aborts the ENTIRE Emacs process (SIGABRT inside
;;   `pango_core_text_font_map_load_fontset') the moment a glyph falls back to
;;   a color-emoji font.  The abort happens deep in a C library and cannot be
;;   caught with `condition-case', so the only defense is to never hand emoji
;;   to librsvg in the first place.
;;
;;   `markdown-table-preview' and `markdown-mermaid' both feed user text into
;;   SVG <text>, so both sanitize through here.  `markdown-math' is unaffected:
;;   MathJax emits glyph <path>s, never <text>.

;;; Code:

(defgroup markdown-emoji nil
  "Keep color-emoji out of librsvg-rendered SVG text."
  :group 'markdown)

(defcustom markdown-emoji-placeholder "□"
  "Glyph substituted for each emoji cluster in rendered SVG text.
Must itself be a plain (non-color) glyph, or it would trigger the very
crash it guards against: `□' (U+25A1) is safe; avoid `▪'/`▫' and other
characters Unicode flags as emoji."
  :type 'string
  :group 'markdown-emoji)

(defun markdown-emoji--joiner-p (ch)
  "Non-nil if CH forms a color-emoji cluster with the preceding glyph.
Covers the emoji variation selector (U+FE0F), the zero-width joiner, the
combining enclosing keycap, and the skin-tone modifiers.  A plain symbol
such as `⚠' becomes a color emoji only when one of these follows it, so
the caller promotes the preceding glyph to the placeholder."
  (or (= ch #xFE0F) (= ch #x200D) (= ch #x20E3)
      (<= #x1F3FB ch #x1F3FF)))

(defun markdown-emoji--text-selector-p (ch)
  "Non-nil if CH is a variation selector other than U+FE0F.
These request text (non-color) presentation, so they are dropped without
forcing a placeholder."
  (<= #xFE00 ch #xFE0E))

(defun markdown-emoji--base-p (ch)
  "Non-nil if CH renders as a color emoji on its own."
  (or (eq (aref char-script-table ch) 'emoji)
      ;; A handful of media-control emoji (e.g. ⏭ U+23ED) are classified
      ;; `symbol' by `char-script-table' but still render in color.
      (<= #x23E9 ch #x23FA)))

(defun markdown-emoji-sanitize (text)
  "Return TEXT with every emoji cluster replaced by `markdown-emoji-placeholder'.
Non-string TEXT is returned unchanged.  One placeholder is emitted per
emoji: joiners and modifiers collapse into the glyph they follow."
  (if (not (stringp text))
      text
    (let ((res '()))
      (mapc
       (lambda (ch)
         (cond
          ((markdown-emoji--joiner-p ch)
           (when res (setcar res markdown-emoji-placeholder)))
          ((markdown-emoji--text-selector-p ch) nil) ; strip, keep base as-is
          ((markdown-emoji--base-p ch)
           (push markdown-emoji-placeholder res))
          (t (push (char-to-string ch) res))))
       text)
      (apply #'concat (nreverse res)))))

(provide 'markdown-emoji)
;;; markdown-emoji.el ends here
