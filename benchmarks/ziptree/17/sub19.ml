type item = string

type tree = LEAF of item
		  | NODE of tree list

type zipper = TOP
			| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let rec goLeft: location -> location = fun loc ->
	match loc with
	|LOC(t, TOP) -> raise (NOMOVE "left of top")
	|LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	|LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let rec goRight: location -> location = fun loc ->
	match loc with
	|LOC(t, TOP) -> raise (NOMOVE "right of top")
	|LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	|LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of first")

let rec goUp: location -> location = fun loc ->
	match loc with
	|LOC(t, HAND(left, up, right)) -> LOC(NODE (List.append (List.rev left) (t::right)), up)
	|LOC(t, TOP) -> raise (NOMOVE "top of top")

let rec goDown: location -> location = fun loc ->
	match loc with
	|LOC(NODE [], zip) -> raise (NOMOVE "bottom of bottom")
	|LOC(NODE (head::tail), zip) -> LOC(head, HAND([], zip, tail))
	|LOC(LEAF item, zip) -> raise (NOMOVE "leaf")


