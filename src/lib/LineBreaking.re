open Base;

/* Determine if an item can break while keeping a running total of widths */
let determine_item_can_break =
    (item: Item.t, prevItem: option(Item.t), sums: ItemSums.t)
    : (bool, ItemSums.t) =>
  switch (item) {
  | Item.Penalty(penalty) => (Float.(penalty.cost < Const.max_cost), sums)
  | Item.Box(box) => (false, {...sums, width: sums.width +. box.width})
  | Item.Glue(glue) =>
    /* Only breakable if the previous item is a box */
    let canBreak = Option.value_map(prevItem, ~default=false, ~f=Item.is_box);
    let new_sums: ItemSums.t = {
      width: sums.width +. glue.width,
      shrink: sums.shrink +. glue.shrink,
      stretch: sums.stretch +. glue.stretch,
    };
    if (canBreak) {
      (true, new_sums);
    } else {
      (false, new_sums);
    };
  };

type node_set = Set.t(Node.t, Node.comparator_witness);
let empty_node_set = Set.empty((module Node));

/* Determine the ratio of spacing aadjustment needed for a line between an item
   and node to fit in the ideal length, based on the stretch and shrink allowed */
let get_adjustment_ratio =
    (item: Item.t, node: Node.t, sums: ItemSums.t, ideal_width: float): float => {
  let line_shrink = sums.shrink -. node.sums.shrink;
  let line_stretch = sums.stretch -. node.sums.stretch;
  let line_width =
    switch (item) {
    | Penalty({width, _}) => sums.width -. node.sums.width +. width
    | _ => sums.width -. node.sums.width
    };

  if (Float.(line_width < ideal_width)) {
    (ideal_width -. line_width) /. line_stretch;
  } else {
    (ideal_width -. line_width) /. line_shrink;
  };
};

let get_fitness = (adjustment_ratio: float): int =>
  Float.(
    if (adjustment_ratio < (-0.5)) {
      0;
    } else if (adjustment_ratio < 0.5) {
      1;
    } else if (adjustment_ratio < 1.0) {
      2;
    } else {
      3;
    }
  );

/*
   When breaking a line, we don't want to include the width of any glue or
   non-breakable penalty items in the next breakpoint. When rendering a line,
   we ignore leading glue and penalty items.

   This function determines the width of any following glue and non-breakable
   penalties and adds them to the current sums so they're ignored on the next
   iteration.
 */
let rec get_sums_to_box = (items: list(Item.t), sums: ItemSums.t): ItemSums.t =>
  Float.(
    switch (items) {
    | []
    | [Box(_), ..._] => sums
    | [Penalty({cost, _}), ..._] when cost >= Const.max_cost => sums
    | [Penalty({width, _}), ...xs] =>
      get_sums_to_box(xs, {...sums, width: sums.width +. width})
    | [Glue({width, stretch, shrink}), ...xs] =>
      get_sums_to_box(
        xs,
        {
          width: sums.width +. width,
          stretch: sums.stretch +. stretch,
          shrink: sums.shrink +. shrink,
        },
      )
    }
  );

let get_next_nodes =
    (
      index: int,
      item: Item.t,
      prev_item: option(Item.t),
      next_items: list(Item.t),
      nodes: node_set,
      sums: ItemSums.t,
      ideal_width: float,
    )
    : node_set => {
  /* for the first item in the list, try and create a breakpoint from each node. if not possible, remove node from set */
  let (feasible, next_nodes) =
    Set.fold(
      nodes,
      ~init=(empty_node_set, nodes),
      ~f=((feasible, nodes), node) => {
        let adjustment_ratio =
          get_adjustment_ratio(item, node, sums, ideal_width);

        /*
           TODO: figure out how to properly deal with this NaN.
           The stretch value for the end of line glue is infinity which causes
           a bunch of super weird issues that I have no idea how to fix
         */
        if (Float.is_nan(adjustment_ratio)) {
          /* this is probably a forced break */
          let new_node: Node.t = {
            position: index,
            item,
            line: node.line + 1,
            parent: Some(node),
            adjustment_ratio,
            sums,
            fitness: 1,
            totalDemerits: node.totalDemerits +. 1000.0,
          };

          (Set.add(feasible, new_node), nodes);
        } else if (Float.(adjustment_ratio < Const.min_adjustment_ratio)
                   || Item.is_forced_break(item)) {
          (
            /*
               All items from node to item are too wide to fit on one line,
               remove this node from the set
             */
            feasible,
            Set.remove(nodes, node),
          );
        } else if (Float.(adjustment_ratio >= Const.min_adjustment_ratio)
                   && Float.(adjustment_ratio <= Const.max_adjustment_ratio)) {
          /*
             This is a feasible breakpoint, so calculate the demerits and add to
             feasible list
           */

          let badness = 100.0 *. (Float.abs(adjustment_ratio) **. 3.0);
          let penalty = Item.penalty_cost(item);
          let penalty_demerits =
            if (Float.(penalty >= 0.0)) {
              (1.0 +. badness +. penalty) **. 2.0;
            } else if (Float.(penalty > Const.min_cost)) {
              (1.0 +. badness) **. 2.0 -. penalty **. 2.0;
            } else {
              (1.0 +. badness) **. 2.0;
            };

          let double_hyphen_demerits =
            Option.value_map(prev_item, ~default=0.0, ~f=prev_item =>
              switch (item, prev_item) {
              | (Penalty({flagged: true, _}), Penalty({flagged: true, _})) => Const.double_hyphen_cost
              | _ => 0.0
              }
            );

          let fitness = get_fitness(adjustment_ratio);
          let fitness_demerits =
            Option.value_map(prev_item, ~default=0.0, ~f=_ =>
              if (Int.abs(fitness - node.fitness) > 1) {
                Const.adjacent_loose_tight_cost;
              } else {
                0.0;
              }
            );

          let demerits =
            penalty_demerits +. double_hyphen_demerits +. fitness_demerits;
          let new_sums = get_sums_to_box(next_items, sums);

          let new_node: Node.t = {
            position: index,
            item,
            line: node.line + 1,
            parent: Some(node),
            adjustment_ratio,
            sums: new_sums,
            fitness,
            totalDemerits: node.totalDemerits +. demerits,
          };

          (Set.add(feasible, new_node), nodes);
        } else {
          (
            /* This line is too short, so just continue with no changes */
            feasible,
            nodes,
          );
        };
      },
    );

  switch (Set.min_elt(feasible)) {
  | Some(new_node) => Set.add(next_nodes, new_node)
  | None => next_nodes
  };
};

let rec determine_breakpoints_iter =
        (
          index: int,
          items: list(Item.t),
          prev_item: option(Item.t),
          nodes: node_set,
          prev_sums: ItemSums.t,
          ideal_width: float,
        )
        : option(Node.t) =>
  switch (items) {
  | [] => Set.min_elt(nodes)
  | [item, ...next_items] =>
    let (canBreak, sums) =
      determine_item_can_break(item, prev_item, prev_sums);

    if (canBreak) {
      let next_nodes =
        get_next_nodes(
          index,
          item,
          prev_item,
          next_items,
          nodes,
          sums,
          ideal_width,
        );

      let next_sums: ItemSums.t =
        switch (item) {
        | Glue({width, stretch, shrink}) => {
            width: sums.width +. width,
            stretch: sums.stretch +. stretch,
            shrink: sums.shrink +. shrink,
          }
        | _ => sums
        };

      determine_breakpoints_iter(
        index + 1,
        next_items,
        Some(item),
        next_nodes,
        next_sums,
        ideal_width,
      );
    } else {
      determine_breakpoints_iter(
        index + 1,
        next_items,
        Some(item),
        nodes,
        sums,
        ideal_width,
      );
    };
  };

let determine_breakpoints =
    (items: list(Item.t), ideal_width: float): option(Node.t) =>
  switch (items) {
  | [] => None
  | [first_item, ..._] =>
    let start_node: Node.t = {
      position: 0,
      item: first_item,
      line: 0,
      parent: None,
      adjustment_ratio: 1.0,
      sums: ItemSums.empty,
      fitness: 0,
      totalDemerits: 0.0,
    };
    let nodes = Set.singleton((module Node), start_node);
    determine_breakpoints_iter(
      0,
      items,
      None,
      nodes,
      ItemSums.empty,
      ideal_width,
    );
  };

type breakpoint = {
  position: int,
  adjustment_ratio: float,
};

type line = {
  text: string,
  adjustment_ratio: float,
};

let rec rollup_nodes =
        (node: Node.t, breaks: list(breakpoint)): list(breakpoint) => {
  let break: breakpoint = {
    position: node.position,
    adjustment_ratio: node.adjustment_ratio,
  };
  let new_breaks = [break, ...breaks];
  switch (node.parent) {
  | Some(parent) when parent.position != 0 =>
    rollup_nodes(parent, new_breaks)
  | _ => new_breaks
  };
};

let get_lines = (items: list(Item.t), breaks: list(breakpoint)): list(line) => {
  let rec iter =
          (
            items: list(Item.t),
            breaks: list(breakpoint),
            lines: list(line),
            line_text: string,
            index: int,
          )
          : list(line) =>
    switch (items, breaks) {
    | ([], _)
    | (_, []) => lines
    | ([item, ...next_items], [break, ...next_breaks]) =>
      if (break.position == index && index != 0) {
        let line = {
          text: line_text,
          adjustment_ratio: break.adjustment_ratio,
        };
        iter(next_items, next_breaks, [line, ...lines], "", index + 1);
      } else {
        let text =
          switch (item) {
          | Box({text, _})
          | Penalty({text, _}) => text
          | Glue(_) => " "
          };

        iter(next_items, breaks, lines, line_text ++ text, index + 1);
      }
    };

  List.rev(iter(items, breaks, [], "", 0));
};
