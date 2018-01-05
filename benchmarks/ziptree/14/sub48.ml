(* hw2-3, 2012-11259 *)

type item = string
type tree = LEAF of item | NODE of tree list
type zipper = TOP | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft: location -> location =
  fun loc -> match loc with
    | LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight: location -> location =
  fun loc -> match loc with
    | LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let goUp: location -> location =
  fun loc -> match loc with
    | LOC(t, TOP) -> raise (NOMOVE "up of top")
	| LOC(t, HAND(left, TOP, right)) -> LOC(NODE((List.rev left)@[t]@right), TOP)
	| LOC(t, HAND(left, HAND(upLeft, up, upRight), right)) ->
	  LOC(NODE((List.rev left)@[t]@right), HAND(upLeft, up, upRight))

let goDown: location -> location =
  fun loc -> match loc with
    | LOC(LEAF i, _) -> raise (NOMOVE "down of bottom")
    | LOC(NODE l, z) -> match l with
	  | [] -> raise (NOMOVE "down of bottom")
	  | hd::tl -> LOC(hd, HAND([], z, tl))
