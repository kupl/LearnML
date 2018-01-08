exception NOMOVE of string

type tree = LEAF of item
| NODE of tree list
and item = string

type zipper = TOP
| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc = match loc with
LOC(t, TOP) -> raise (NOMOVE "left of top")
| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
LOC(t, TOP) -> raise (NOMOVE "Right of top")
| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
| LOC(t, HAND(left,up,[])) -> raise (NOMOVE "Right of last")

let goUp loc = match loc with
LOC(t, TOP) -> raise (NOMOVE "Up of top")
| LOC(t, HAND(left, up, right)) -> LOC ((NODE((List.rev left)@[t]@right)), up)

let goDown loc = match loc with
LOC(LEAF(k), _) -> raise (NOMOVE "Down of leaf")
| LOC(NODE(a::h), hand) -> LOC(a, HAND([], hand, h))
| LOC(NODE([]), _) -> raise (NOMOVE "Down of node without child")

let a = LOC (LEAF("*"),
HAND([LEAF ("c")],
HAND([LEAF ("+"); NODE [LEAF ("a"); LEAF ("*"); LEAF ("b")]],
TOP,
[]),
[LEAF ("d")]))