type item = string
type tree = LEAF of item
          | NODE of tree list
type zipper = TOP
            | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
exception NOMOVE of string

let goLeft (loc: location) : location =
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight (loc: location) : location =
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "right of top")
  | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let goUp (loc: location) : location =
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "top of tree")
  | LOC(t, HAND(left, up, right)) -> LOC(NODE((List.rev left)@(t::right)), up)

let goDown (loc: location) : location =
  match loc with
  | LOC(LEAF t, TOP) -> raise (NOMOVE "only one leaf tree")
  | LOC(LEAF t, HAND(left, up, right)) -> raise (NOMOVE "bottom of tree")
  | LOC(NODE (t::n), h) -> LOC(t, HAND([], h, n))
  | LOC(NODE [], _) -> raise (NOMOVE "no way to go")
