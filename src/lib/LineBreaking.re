open Base;

module Node = {
  type box = {
    width: float,
    text: string,
  };

  type glue = {
    width: float,
    stretchability: float,
    shrinkability: float,
  };

  type penalty = {
    width: float,
    cost: float,
    flagged: bool,
  };

  type t =
    | Box(box)
    | Glue(glue)
    | Penalty(penalty);
};

type node = Node.t;

let end_of_paragraph = [
  Node.Glue({width: 0.0, stretchability: Float.infinity, shrinkability: 0.0}),
  Node.Penalty({width: 0.0, cost: Float.neg_infinity, flagged: true}),
];

let paragraph_to_nodes = (paragraph: string): list(node) => {
  let words = String.split(paragraph, ~on=' ');
  let numWords = List.length(words);

  List.concat_mapi(
    words,
    ~f=(i, word) => {
      let box =
        Node.Box({width: Float.of_int(String.length(word)), text: word});

      if (i == numWords - 1) {
        [box, ...end_of_paragraph];
      } else {
        [box];
      };
    },
  );
};
