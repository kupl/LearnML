type item = string

type tree = 
	| LEAF of item
	| NODE of tree list

type zipper = 
	| TOP
	| HAND of tree list * zipper * tree list

type location = 
	| LOC of tree * zipper

exception NOMOVE of string
let goLeft loc = 
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc =
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goDown loc =
	match loc with
	| LOC(LEAF _, _) -> raise (NOMOVE "no children")
	| LOC(NODE [], up) -> raise (NOMOVE "empty")
	| LOC(NODE (t1::t2t3), up) -> LOC(t1, HAND([], up, t2t3))

let rec merge (l1, l2) =
	match (l1, l2) with
	| ([], _) -> l2
	| (_, []) -> l1
	| (h::t, _) -> merge(t, h::l2)


let goUp loc = 
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "no parent")
	| LOC(t, HAND(left, up, right)) -> LOC(NODE(merge(left, t::right)), up)
