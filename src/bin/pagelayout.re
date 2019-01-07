open Stdio;

open Lib;

let lipsum = "Lorem Ipsum is simply dummy text of the printing and typesetting \
 industry. Lorem Ipsum has been the industry's standard dummy text ever since \
 the 1500s, when an unknown printer took a galley of type and scrambled it to \
 make a type specimen book. It has survived not only five centuries, but also \
 the leap into electronic typesetting, remaining essentially unchanged. It was \
 popularised in the 1960s with the release of Letraset sheets containing Lorem \
 Ipsum passages, and more recently with desktop publishing software like Aldus \
 PageMaker including versions of Lorem Ipsum.";

let items = Item.from_paragraph(lipsum, 12.0);
let possible_node = LineBreaking.determine_breakpoints(items, 400.0);

switch (possible_node) {
| None => printf("well crap it didn't find anything\n")
| Some(node) =>
  let breaks = LineBreaking.rollup_nodes(node, []);
  let lines = LineBreaking.get_lines(items, breaks);

  List.iter(
    ({text, adjustment_ratio}: LineBreaking.line) =>
      printf("% 5.2f - %s\n", adjustment_ratio, text),
    lines,
  );
};
