(*2011-11004 ³²À±¼® ¹®Á¦ 3*)

exception NOMOVE of string
type item = string
type tree = LEAF of item
	| NODE of tree list
type zipper = TOP
	| HAND of tree list * zipper * tree list
type lcation = LOC of tree * zipper

let goLeft loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let goUp loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "top")
	| LOC(t, HAND(left, TOP, right)) -> LOC(NODE[NODE(List.rev left); t; NODE(right)], TOP)
	| LOC(t, HAND(left, HAND(l, z, r), right)) -> LOC(NODE[NODE(List.rev left); t; NODE(right)], HAND(l, z, r))

let goDown loc = match loc with
	LOC(LEAF x, _) -> raise (NOMOVE "bottom")
	| LOC(NODE([]), _) -> raise (NOMOVE "bottom")
	| LOC(NODE(l::bot), TOP) -> LOC(l, HAND([], TOP , bot)) 
	| LOC(NODE(l::bot), HAND(left, up, right)) -> LOC(l, HAND([], HAND(left, up, right) , bot)) 




