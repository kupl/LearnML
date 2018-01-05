(* hw2-3 *)

type item = string
type tree =
  | LEAF of item
  | NODE of tree list
type zipper =
  | TOP
  | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
exception NOMOVE of string

let goLeft loc =
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "left of top")
  | LOC (t, HAND (l::left, up, right)) -> LOC (l, HAND (left, up, t::right))
  | LOC (t, HAND ([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc =
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "right of top")
  | LOC (t, HAND (left, up, r::right)) -> LOC (r, HAND (t::left, up, right))
  | LOC (t, HAND (left, up, [])) -> raise (NOMOVE "right of last")

let goUp loc =
  match loc with
  | LOC (t, TOP) -> raise (NOMOVE "up of top")
  | LOC (t, HAND (left, up, right)) ->
    LOC (NODE (List.rev_append left (t::right)), up)

let goDown loc =
  match loc with
  | LOC (LEAF i, h) -> raise (NOMOVE "down of bottom")
  | LOC (NODE (lh::lt), h) -> LOC (lh, HAND ([], h, lt))
  | LOC (NODE [], h) -> raise (NOMOVE "down of no child")
