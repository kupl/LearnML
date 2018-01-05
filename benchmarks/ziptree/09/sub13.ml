(*2006-11720 2-4 KimEunSol*)
exception NOMOVE of string

type tree = LEAF of string | NODE of tree list
type zipper = TOP | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goLeft loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "left of top")
	|LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	|LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "right of top")
	|LOC(t, HAND(l, up, r::right)) -> LOC(r, HAND(t::l, up, right))
	|LOC(t, HAND(l, up, [])) -> raise (NOMOVE "right of first")

let goUP loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "top of top")
	|LOC(t, HAND(l, TOP, r)) -> raise (NOMOVE "top of first")
	|LOC(t, HAND(l, HAND(ll, up, rr),r)) -> LOC(NODE(List.append(t::l)r), HAND(ll, up, rr))
let goDown loc = match loc with
	LOC(NODE(t::rr), HAND(l, up, r)) -> LOC(t, HAND([], HAND(l, up, r), rr))
	|LOC(LEAF t, HAND(l, up, r)) -> raise (NOMOVE "down of first")
	|LOC(LEAF t, TOP) -> raise (NOMOVE "couldn't move")
