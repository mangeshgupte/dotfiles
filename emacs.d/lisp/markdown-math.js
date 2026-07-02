#!/usr/bin/env node
// markdown-math.js -- render a LaTeX math string to a standalone SVG via MathJax 3.
//
// Reads the TeX source from stdin and writes an SVG document to stdout.
// Argv: <mode> <color> <ex-px>
//   mode   "display" (block, $$...$$) or "inline" ($...$)
//   color  any CSS color; used to resolve the glyphs' `currentColor'
//   ex-px  pixels per MathJax `ex' unit -- the root <svg> is sized in ex,
//          so we convert to px for predictable rendering under Emacs/librsvg.
//
// Requires `mathjax-full' to be resolvable.  The Emacs caller sets NODE_PATH
// to the global node_modules dir (see `markdown-math-node-modules-dir'), so
// `npm install -g mathjax-full' is enough -- no local package needed.

const [, , modeArg = "display", color = "currentColor", exPxArg = "8"] =
  process.argv;
const display = modeArg !== "inline";
const exPx = parseFloat(exPxArg) || 8;

function readStdin() {
  return new Promise((resolve, reject) => {
    let data = "";
    process.stdin.setEncoding("utf8");
    process.stdin.on("data", (c) => (data += c));
    process.stdin.on("end", () => resolve(data));
    process.stdin.on("error", reject);
  });
}

(async () => {
  const tex = (await readStdin()).trim();
  if (!tex) process.exit(1);

  const { mathjax } = require("mathjax-full/js/mathjax.js");
  const { TeX } = require("mathjax-full/js/input/tex.js");
  const { SVG } = require("mathjax-full/js/output/svg.js");
  const { liteAdaptor } = require("mathjax-full/js/adaptors/liteAdaptor.js");
  const { RegisterHTMLHandler } = require("mathjax-full/js/handlers/html.js");
  const { AllPackages } = require("mathjax-full/js/input/tex/AllPackages.js");

  const adaptor = liteAdaptor();
  RegisterHTMLHandler(adaptor);
  const texIn = new TeX({ packages: AllPackages });
  const svgOut = new SVG({ fontCache: "local" });
  const doc = mathjax.document("", { InputJax: texIn, OutputJax: svgOut });

  const node = doc.convert(tex, { display });
  let svg = adaptor.innerHTML(node);

  // Bail out (non-zero) on a MathJax parse error so the caller keeps the
  // source visible instead of showing a red error image.
  if (/data-mjx-error|class="mjx-merror"|<merror/.test(svg)) {
    process.stderr.write("mathjax parse error\n");
    process.exit(2);
  }

  // MathJax sizes the root <svg> in `ex' units; convert to px so librsvg
  // renders at a predictable size.
  svg = svg
    .replace(/width="([\d.]+)ex"/, (_, w) => `width="${(w * exPx).toFixed(2)}px"`)
    .replace(/height="([\d.]+)ex"/, (_, h) => `height="${(h * exPx).toFixed(2)}px"`);

  // Resolve `currentColor' fills to the requested color.  MathJax already
  // puts a style="vertical-align:..." on the root <svg>; extend it if
  // present, otherwise add one.
  if (/<svg[^>]*\sstyle="/.test(svg)) {
    svg = svg.replace(/(<svg[^>]*\sstyle=")/, `$1color:${color};`);
  } else {
    svg = svg.replace(/<svg /, `<svg style="color:${color}" `);
  }

  process.stdout.write(svg);
})().catch((e) => {
  process.stderr.write(String((e && e.stack) || e) + "\n");
  process.exit(1);
});
