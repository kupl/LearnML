type item = string
type tree = LEAF of item
          | NODE of tree list
(* so, it can have multiple children. *)
type zipper = TOP
            | HAND of tree list * zipper * tree list
(* this zipper thing has following property : HAND(l, z, r) refers to... 
l = left siblings, from the nearest.
z = the parent zipper, where this zipper will go up.
r = right siblings, from the nearest*)
type location = LOC of tree * zipper
(* LOC(t, z) refers to...
t = the tree which this location becomes the root. The very tree starting from here.
z = the zipper which has this location as children. *)

exception NOMOVE of string

let goLeft loc = match loc with
  | LOC(_, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([],up,right)) -> raise(NOMOVE "left of first")

  let goRight loc = match loc with
  | LOC(t, TOP) -> raise (NOMOVE "right of top")
  | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  | LOC(t, HAND(left,up,[])) -> raise(NOMOVE "right of first")

  let goUp loc = match loc with
  | LOC(t, TOP) -> raise (NOMOVE "up of top")
  | LOC(t, HAND(left, up, right)) -> LOC(NODE(List.rev_append(left)(t :: right)), up)

  let goDown loc = match loc with
  | LOC(LEAF(_), _) -> raise (NOMOVE "down of leaf")
  | LOC(NODE(l::t), curzip) -> LOC(l, HAND([], curzip, t))
  | LOC(NODE([]), _) -> raise (NOMOVE "empty node : probably construction error")
