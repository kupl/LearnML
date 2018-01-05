exception NOMOVE of string

type item = string
type tree =
  | LEAF of item
  | NODE of tree list

type zipper =
  | TOP
  | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc =
  match loc with
  | LOC (curr_t, TOP) -> raise (NOMOVE "left of top") (* OK *)
  | LOC (curr_t, HAND ([], up, right)) -> raise (NOMOVE "left of first") (* OK *)
  | LOC (curr_t, HAND (next_t::next_left, up, right)) -> LOC (next_t, HAND (next_left, up, curr_t::right)) (* OK *)

let goRight loc =
  match loc with
  | LOC (curr_t, TOP) -> raise (NOMOVE "right of top") (* OK *)
  | LOC (curr_t, HAND (left, up, [])) -> raise (NOMOVE "right of last") (* OK *)
  | LOC (curr_t, HAND (left, up, next_t::next_right)) -> LOC (next_t, HAND (curr_t::left, up, next_right)) (* OK *)

let goUp loc =
  match loc with
  | LOC (curr_t, TOP) -> raise (NOMOVE "up of top")
  | LOC (curr_t, HAND (left, up, right)) -> LOC (NODE (List.rev left @ curr_t::right), up)

let goDown loc =
  match loc with
  | LOC (LEAF _, zip) -> raise (NOMOVE "down of leaf")
  | LOC (NODE [], zip) -> raise (NOMOVE "down of empty node")
  | LOC (NODE (next_t::next_right_sib), zip) -> LOC (next_t, HAND ([], zip, next_right_sib))
