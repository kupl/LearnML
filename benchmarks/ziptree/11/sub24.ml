(*2009-11718 1-8*)

exception NOMOVE of string
type tree = LEAF of item
	| NODE of tree list
and item= string
type zipper = TOP
	| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goLeft loc = match loc with
	LOC (t, TOP) -> raise (NOMOVE "left of top")
	| LOC (t, HAND(l::left, up, right)) -> LOC (l, HAND(left, up, t::right))
	| LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
	LOC (t, TOP) -> raise (NOMOVE "right of top")
	| LOC (t, HAND(left, up, l::right)) -> LOC (l, HAND(t::left, up, right))
	| LOC (t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp loc = match loc with
	LOC (t, TOP) -> raise (NOMOVE "it's top")
	| LOC (t, HAND(left, HAND(up_left,up,up_right) , right)) ->
		 LOC (NODE ((List.rev left)@[t]@right) , HAND(up_left, up, up_right))
	| LOC (t, HAND(left, TOP, right)) ->
		LOC (NODE ((List.rev left)@[t]@right) , TOP)

let goDown loc = match loc with
	LOC (LEAF a, _) -> raise (NOMOVE "it's leaf")
	| LOC (NODE (down_left::tl), HAND(left, up, right)) -> LOC (down_left, HAND([], HAND(left,up,right), tl))
	| LOC (NODE (down_left::tl), TOP) -> LOC (down_left, HAND([], TOP, tl))
	| LOC (NODE _, _) -> raise (NOMOVE "it's bottom")
