exception NOMOVE of string

type item = string

type tree = LEAF of item
	| NODE of tree list

type zipper = TOP | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goRight loc =
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp loc =
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "up of top")
	| LOC(t, HAND([],z,[])) -> LOC(NODE([t]),z)
	| LOC(t, HAND(l,z,[])) -> LOC(NODE((List.rev l)@[t]),z)
	| LOC(t, HAND([],z,r)) -> LOC(NODE([t]@r),z)
	| LOC(t, HAND(l,z,r)) -> LOC(NODE((List.rev l)@[t]@r),z)

let goDown loc =
	match loc with
	LOC(LEAF(_),_) -> raise (NOMOVE "down of leaf")
	| LOC(NODE(h::t), z) -> LOC(h, HAND([], z, t))
	| LOC(NODE [],_) -> raise (NOMOVE "tree is NODE []")