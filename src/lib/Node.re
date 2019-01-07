open Base;

module T = {
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

  let compare = (t1: t, t2: t): int => {
    let cmp_demerits = Float.compare(t1.totalDemerits, t2.totalDemerits);
    if (cmp_demerits != 0) {
      Int.compare(t1.position, t2.position);
    } else {
      cmp_demerits;
    };
  };

  let sexp_of_t = (t: t): Sexp.t => Int.sexp_of_t(t.position);
};

include T;
include Comparator.Make(T);
