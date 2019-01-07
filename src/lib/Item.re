open Base;

module T = {
  type box = {
    width: float,
    text: string,
  };

  type glue = {
    width: float,
    shrink: float,
    stretch: float,
  };

  type penalty = {
    width: float,
    text: string,
    cost: float,
    /* flag if undesirable to have multiple breaks like this in a row */
    flagged: bool,
  };

  type t =
    | Box(box)
    | Glue(glue)
    | Penalty(penalty);
};

include T;

let is_box = (item: t): bool =>
  switch (item) {
  | Box(_) => true
  | _ => false
  };

let is_forced_break = (item: t): bool =>
  switch (item) {
  | Penalty({cost, _}) => Float.(cost <= Const.min_cost)
  | _ => false
  };

let penalty_cost = (item: t): float =>
  switch (item) {
  | Penalty(penalty) => penalty.cost
  | _ => 0.0
  };

let end_of_paragraph = [
  Glue({width: 0.0, stretch: Const.max_cost, shrink: 0.0}),
  Penalty({width: 0.0, text: "", cost: Const.min_cost, flagged: false}),
];

let from_paragraph = (paragraph: string, fontSize: float): list(t) => {
  let words = String.split(paragraph, ~on=' ');
  let numWords = List.length(words);

  List.concat_mapi(
    words,
    ~f=(i, word) => {
      let box =
        Box({
          width: Float.of_int(String.length(word)) *. fontSize,
          text: word,
        });

      let glue =
        Glue({
          width: fontSize,
          stretch: fontSize *. 1.3,
          shrink: fontSize *. 0.7,
        });

      if (i == numWords - 1) {
        [box, ...end_of_paragraph];
      } else {
        [box, glue];
      };
    },
  );
};
