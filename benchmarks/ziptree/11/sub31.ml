type tree = LEAF of item
	| NODE of tree list
type zipper = TOP
	| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper


let goLeft loc = match loc with
	LOC(t, TOP) -> raise NOMOVE "left of top"
	| LOC(t, HAND(l::left, up, right)) -> LOC (l, HAND(left, up, t::right))
	| LOC(t, HAND([], up, right)) -> raise NOMOVE "left of first"

let goRight loc = match loc with
	LOC(t, TOP) -> raise NOMOVE "top of tree"
	| LOC(t, HAND(

let goUP loc = match loc with

let goDown = match loc with
