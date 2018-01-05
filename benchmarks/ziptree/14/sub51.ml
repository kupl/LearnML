exception NOMOVE of string 
exception NOGET of string

type item = string
type tree = LEAF of item
		| NODE of tree list

type zipper = TOP
		| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let getL = function TOP -> raise (NOGET "left of top")
				| HAND (l,_,_) -> l
let getR = function TOP -> raise (NOGET "right of top")
				| HAND (_,_,r) -> r
let getU = function TOP -> raise (NOGET "up of top")
				| HAND (_,u,_) -> u

let goLeft loc = match loc with
		LOC(t, TOP) -> raise (NOMOVE "left of top")
		| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
		| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
		LOC(t,TOP) -> raise (NOMOVE "right of top")
		| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
		| LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of last")

let goUp loc = match loc with
		LOC(t, TOP) -> raise (NOMOVE "up of top")
		| LOC(t, HAND(left, TOP, right)) -> LOC(NODE (left@[t]@right), TOP)
		| LOC(t, HAND(left, up, right)) -> LOC(NODE (left@[t]@right), HAND(getL up,getU up,getR up))

let goDown loc = match loc with
		LOC(NODE (h::t), zip) -> LOC(h, HAND([],zip,t))
		| LOC(NODE [], zip) -> raise (NOMOVE "empty node")
		| LOC(LEAF _, zip) -> raise (NOMOVE "down of bottom")
