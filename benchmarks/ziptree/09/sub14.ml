(* 2006-11782 Song Young-chan, Hw2-4 zipper *)

exception Error of string

type tree = LEAF of item
	  | NODE of tree list
and item = string

type zipper = TOP
	    | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc =
	match loc with
	  LOC(t, TOP) -> raise (Error "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([], up, right)) -> raise (Error "left of first")

let goRight loc =
	match loc with
	  LOC(t, TOP) -> raise (Error "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (Error "right of first")

let goUp loc =
	match loc with
	  LOC(t, TOP) -> raise (Error "now location is top")
	| LOC(t, HAND(left, zip, right)) -> LOC(NODE((List.rev left)@(t::right)), zip)

let goDown loc =
	match loc with
	  LOC(LEAF(a), zip) -> raise (Error "now location is bottom")
	| LOC(NODE([]), zip) -> raise (Error "down is not exist")
	| LOC(NODE(h::t), zip) -> LOC(h, HAND([], zip, t))
