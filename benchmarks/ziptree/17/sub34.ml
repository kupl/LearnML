type item = string

type tree = 	LEAF of item
		| NODE of tree list

type zipper = 	TOP
		| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc = 
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC (l, HAND(left, up, t::right))
	| LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc =
	match loc with
	| LOC (t, TOP) -> raise (NOMOVE "right of top")
	| LOC (t, HAND(left, up, r::right)) -> LOC (r, HAND(t::left, up, right))
	| LOC (t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let goUp loc =
	match loc with
	| LOC (t, TOP) -> raise (NOMOVE "on top")
	| LOC (t, HAND(l::left, up, right)) -> LOC (NODE(List.append left 
							(List.append (l::[t]) right)), up)
	| LOC (t, HAND([], up, right)) -> LOC (NODE(t::right), up)

let goDown loc =
	match loc with
	| LOC (LEAF it, _) -> raise (NOMOVE "on bottom")
	| LOC (NODE [], _) -> raise(NOMOVE "on bottom")
 	| LOC (NODE (h::t), zip) -> LOC (h, HAND([], zip, t))
		
