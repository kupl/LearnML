(*Lee Seok Jin 2013-11417 CSE hw2_5*)

exception NOMOVE of string

type item	= string

type tree 	= LEAF of item
		| NODE of tree list

type zipper	= TOP
		| HAND of tree list * zipper * tree list

type location 	= LOC of tree * zipper

let goLeft(loc: location): location = 
	match loc with 
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight(loc: location): location = 
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "left of first")

let goUp(loc: location): location = 
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "Most Top")
	| LOC(t, HAND(left, up, right)) -> LOC(NODE(List.rev(left)@[t]@right), up)

let goDown(loc: location): location =
	match loc with
	| LOC(LEAF(_),_) -> raise (NOMOVE "Most down")
	| LOC(NODE([]),_) -> raise (NOMOVE "Most down")
	| LOC(NODE(h::t), up) -> LOC(h, HAND([],up,t))
