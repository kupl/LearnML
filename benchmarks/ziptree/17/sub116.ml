
type item = string
type tree = LEAF of item
          | NODE of tree list

type zipper = TOP
            | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string


let goLeft loc = match loc with
    LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
    LOC(t, TOP) -> raise (NOMOVE "right of top")
  | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  | LOC(t, HAND(_, _, [])) -> raise (NOMOVE "right of last")

let merge : tree list -> tree -> tree list -> tree list = fun l t r ->
  let rec mergelr l r = match l with
      [] -> r
    | e :: l_ -> mergelr l_ (e :: r)
  in mergelr l (t :: r)

let goUp loc = match loc with
    LOC(t, TOP) -> raise (NOMOVE "top of top")
  | LOC(t, HAND(left, z, right)) -> LOC(NODE (merge left t right), z)

let goDown loc = match loc with
    LOC(LEAF _, _) -> raise (NOMOVE "down of leaf")
  | LOC(NODE [], _) -> raise (NOMOVE "down of empty node")
  | LOC(NODE (l::sib), z) -> LOC(l, HAND([], z, sib))

