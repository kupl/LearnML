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
	| LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of first")

let goUp loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "no more top")
	| LOC (t, HAND(l1, z, l2)) -> LOC(NODE( List.rev_append (t::l1) l2 ), z)

let goDown loc = 
	let LOC(t, z) = loc in
	match t with
	LEAF i -> raise (NOMOVE "no more down")
	| NODE l -> LOC(List.hd l, HAND([], z, List.tl l))