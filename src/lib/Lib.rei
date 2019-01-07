module Item: {
  type t;
  let from_paragraph: (string, float) => list(t);
};

module Node: {
  type t = {
    /* index of the breakpoint in a node list */
    position: int,
    /* the item at the index */
    item: Item.t,
    /* line number */
    line: int,
    /* previous best breakpoint */
    parent: option(t),
    /* ratio to multiply word spaces */
    adjustment_ratio: float,
    /* total width/stretch/shrink up to and including self */
    sums: ItemSums.t,
    /* a measure of how spaced a line is */
    fitness: int,
    /* parent total demerits + this breakpoint demerits */
    totalDemerits: float,
  };
};

module LineBreaking: {
  type breakpoint;
  type line = {
    text: string,
    adjustment_ratio: float,
  };

  let determine_breakpoints: (list(Item.t), float) => option(Node.t);
  let rollup_nodes: (Node.t, list(breakpoint)) => list(breakpoint);
  let get_lines: (list(Item.t), list(breakpoint)) => list(line);
};
