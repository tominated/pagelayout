I really like the concepts found in Adobe InDesign for page layout and desktop
publishing (the frames system, styles, etc), and wanted to make a DSL to create
documents similar to InDesign, but with the line-breaking algo found in LaTeX.

The end goal is for this project to be able to generate a nicely typeset PDF,
based on a document you define with the DSL. It'll have pages, frames, styles
and text/image content that fits in frames. I'm hoping to allow custom fonts
and their stylistic flourishes like ligatures.

As of the 6th of Jan (commit fa64aa8), I've managed to get a version of the
Knuth-Plass line breaking algorithm working. I'll admit I'm pretty proud that I
managed to do it immutably using recursion.
