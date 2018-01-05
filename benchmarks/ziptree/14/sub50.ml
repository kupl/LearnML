(* hw 2-3 *)
(* 2012-11269 DongJae Lim *)

exception NOMOVE of string

type item = string
type tree = LEAF of item
          | NODE of tree list

type zipper = TOP
            | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc = match loc with
    LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight (loc : location) : location =
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "right of top")
  | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")
  | LOC(t, HAND(left, up, rh::rr)) -> LOC(rh, HAND(t::left, up, rr))

let goUp (loc : location) : location =
  match loc with
  | LOC(t, TOP) -> raise (NOMOVE "up of top")
  | LOC(t, HAND(left, up, right)) -> LOC(NODE((List.rev left)@(t::right)), up)

let goDown (loc : location) : location =
  match loc with
  | LOC(LEAF(_), hand) -> raise (NOMOVE "down of bottom")
  | LOC(NODE([]), hand) -> raise (NOMOVE "down of bottom")
  | LOC(NODE(th::tr), hand) -> LOC(th, HAND([], hand, tr))
