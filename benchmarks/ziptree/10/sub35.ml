exception NOMOVE of string
type item = string
type tree = LEAF of item | NODE of tree list
type zipper = TOP | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goLeft loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "left of top")
	|LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left,up,t::right))
	|LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "right of top")
	|LOC(t, HAND(left, up,r::right)) -> LOC(r, HAND(t::left, up, right))
	|LOC(t, HAND(left, up,[])) -> raise (NOMOVE "right of last")

let goUp loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "up of top")
	|LOC(t, HAND(left, TOP, right)) -> LOC( NODE ((List.append left (t::right))) , TOP)
	|LOC(t, HAND(left, HAND (left_tree, up, right_tree), right) ) -> LOC( NODE (List.append left (t::right)), HAND( left_tree, up, right_tree))

let goDown loc = match loc with
	LOC(l, zip) -> (match l with
		       LEAF a -> raise (NOMOVE "down of leaf")
		       |NODE (h::t) -> LOC(h, HAND([], zip, t))
		       |NODE ([]) -> raise (NOMOVE "invalid"))
