
(* Author: Arif Jafer, 2012-11255 *)
(* PL, Spring 2014 *)

(* HW2-Q3: Zipper *)

type item = string
type tree =
  | LEAF of item
  | NODE of tree list

type zipper =
  | TOP
  | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc = match loc with
  | LOC(t, TOP) -> raise (NOMOVE "left of TOP")
  | LOC(t, HAND([], z, right)) -> raise (NOMOVE "left of leftmost element")
  | LOC(t, HAND(l::left, z, right)) -> LOC(l, HAND(left, z, t::right))

let goRight loc = match loc with
  | LOC(t, TOP) -> raise (NOMOVE "right of TOP")
  | LOC(t, HAND(left, z, [])) -> raise (NOMOVE "right of rightmost element")
  | LOC(t, HAND(left, z, r::right)) -> LOC(r, HAND(t::left, z, right))

let goUp loc = match loc with
  | LOC(t, TOP) -> raise (NOMOVE "goUp of TOP")
  | LOC(t, HAND(left, z, right)) -> 
      LOC(NODE((List.append (List.rev_append left
                                         [t;]) right)), z)

let goDown loc = match loc with
  | LOC(LEAF i, _) -> raise (NOMOVE "goDown of Leaf")
  | LOC(NODE [], _) -> raise (NOMOVE "empty Node")
  | LOC(NODE(x::xs), z) -> LOC(x, HAND([], z, xs))


