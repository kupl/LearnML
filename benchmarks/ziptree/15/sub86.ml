(*
  CSE/2015-21233/김종권
  Homework 2-4
*)
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
  | LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight = function
  | LOC(t, TOP) -> raise (NOMOVE "right of top")
  | LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
  | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp = function
  | LOC(t, TOP) -> raise (NOMOVE "up of top")
  | LOC(t, HAND(left, up, right)) ->
    let rev_left = List.rev left in
    let add_right = t :: right in
    let new_node = rev_left @ add_right in
    LOC(NODE(new_node), up)
                                    
let goDown = function
  | LOC((LEAF _), _) -> raise (NOMOVE "down of bot")
  | LOC(NODE([]), _) -> raise (NOMOVE "down of bot")
  | LOC(NODE(l::tlist), z) -> LOC(l, HAND([], z, tlist))
