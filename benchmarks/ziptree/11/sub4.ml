
type item = string
exception NOMOVE of string

type tree = LEAF of item
		  | NODE of tree list

type zipper = TOP
			| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc = match loc with
      LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
	  LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(r::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let goUP loc = match loc with
	  LOC(t, TOP) -> raise (NOMOVE "TOP")
	| LOC(t, HAND(left, up, right)) -> LOC (NODE((List.rev (t::left))@right), up)

let goDown loc = match loc with
	  LOC(LEAF(t), _) -> raise (NOMOVE "LEAF")
	| LOC(NODE (t::r), HAND(left, up, right)) -> LOC(t, HAND([], HAND(left, up, right), r))
	| LOC(NODE (t::r), TOP) -> LOC(t, HAND([], TOP, r))
	| LOC(NODE[], _) -> raise (NOMOVE "wrong NODE type")
