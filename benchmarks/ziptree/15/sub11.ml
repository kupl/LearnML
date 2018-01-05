exception NOMOVE of string

type item = string

type tree = LEAF of item
		  | NODE of tree list

type zipper = TOP
			| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc =
	match loc with
    | LOC(t, TOP) -> raise (NOMOVE "left of top")
    | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
    | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc =
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "right of top")
    | LOC(t, HAND(left, up, l::right)) -> LOC(l, HAND(t::left, up, right))
    | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let goUp loc =
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "up of top")
(**)	| LOC(t, HAND(left, up, right)) -> NOMOVE "default value"

let goDown loc =
	match loc with
	| LOC(LEAF _, _) -> raise (NOMOVE "down of leaf")
	| LOC(NODE [], _) -> raise (NOMOVE "down of empty node")
	| LOC(NODE (h::l), zip) -> LOC (h, HAND([], zip, l))