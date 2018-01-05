(* 컴퓨터공학부 2009-11833 창배성 *)
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
	| LOC(t, HAND(left, up, r::right)) -> LOC (r, HAND(r::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise NOMOVE "right of tree"

let goUP loc = match loc with
	LOC (t, TOP) -> raise NOMOVE  "top of tree"
	| LOC (t, HAND(left, HAND( sth, sth1, sth2 ), right)) -> LOC (HAND(sth, sth1, sth2), HAND(sth, sth1, sth2))
	| LOC (t, HAND(left, TOP, right)) -> LOC (HAND(left, TOP, right), HAND([],TOP,[])))

let goDown = match loc with
	LOC (HAND( sth, sth1, sth2), blah) -> LOC (sth, HAND(sth, sth1, sth2))
	| LOC (t, blah1) -> raise NOMOVE " bottom of tree"