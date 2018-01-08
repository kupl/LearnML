type item = string
type tree = LEAF of item
	| NODE of tree list
type zipper = TOP
	| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc = 
	match loc with
	|LOC(t,TOP) -> raise (NOMOVE "Left of top")
	|LOC(t,HAND(l::left , up , right)) -> LOC(l, HAND(left, up , t::right))
	|LOC(t,HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc = 
	match loc with
	|LOC(t,TOP) -> raise (NOMOVE "Right of top")
	|LOC(t,HAND(left , up , r::right)) -> LOC(r, HAND(t::left, up , right))
	|LOC(t,HAND(left,up,[])) -> raise (NOMOVE "right of first")

let goUp loc = 
	match loc with
	|LOC(t,TOP) -> raise (NOMOVE "Top of top")
	|LOC(t,HAND(left,up,right)) -> LOC(NODE ((List.rev left)@(t::right)), up)

let goDown loc =
	match loc with
	|LOC(LEAF a,_) -> raise (NOMOVE "Down of bottom")
	|LOC(NODE (hd::tl),now) -> LOC(hd,HAND([],now,tl))
	|LOC(NODE [],_) -> raise (NOMOVE "Empty Node")
