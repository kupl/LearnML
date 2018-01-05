type item = string
type tree = LEAF of item | NODE of tree list
type zipper = TOP | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
exception NOMOVE of string

let goRight loc =
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r , HAND(t::left, up, right))


let goUp loc =
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "up of top")
	| LOC(t, HAND(left, up, right)) -> LOC(NODE((List.rev left)@(t::[])@right), up)


let goDown loc = 
	match loc with
	LOC(LEAF a, up) -> raise (NOMOVE "down of leaf")
	| LOC(NODE [], up) -> raise (NOMOVE "down of EMPTY node")
	| LOC(NODE(left::t), up) -> LOC(left, HAND([], up, t)) 