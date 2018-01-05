type tree = LEAF of item
			|NODE of tree list
		and item = string

type zipper = TOP
			| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc =
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc = 
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let goUp loc = 
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "top")
	| LOC(t, HAND(left, up, right)) -> LOC( (NODE (List.append (List.append left [t]) right)), up)

let goDown loc = 
	match loc with
	LOC(LEAF n, _) -> raise (NOMOVE "bottom")
	| LOC(NODE (h::t), z) -> LOC(h, HAND([], z, t))
	| LOC(NODE [], _) -> raise (NOMOVE "there is no leaf")
