exception NOMOVE of string

type item = string
type tree = LEAF of item
		 	| NODE of tree list

type zipper = TOP
			| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first") 

let goUp loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "top of top")
	| LOC(t, HAND(left, TOP, right)) -> raise (NOMOVE "top of top")
	| LOC(t, HAND(left, HAND(a, up, b), right)) -> 
		LOC(NODE(left@[t]@right), HAND(a, up, b)) 

let goDown loc = match loc with
	LOC(NODE(t::downRight), HAND(left, up, right)) -> LOC(t, HAND([], HAND(left, up, right), downRight)	)
	| LOC(NODE(t::downRight), TOP) -> LOC(t,HAND ([], TOP, downRight))
	| LOC(LEAF t, HAND(left, up, right)) -> raise (NOMOVE "down")
	| LOC(LEAF t, TOP) -> raise (NOMOVE "down")
	| LOC(NODE [], _) -> raise (NOMOVE "down")


