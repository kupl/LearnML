exception NOMOVE of string

type item = string
type tree = LEAF of item
			| NODE of tree list
type zipper = TOP
			| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goLeft loc =
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc =
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp loc =
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "up of top")
	| LOC(t, HAND(left, HAND([], upUp, ru::rightUp), right)) -> LOC(ru, HAND([NODE (List.append left (t::right))], upUp, rightUp))
	| LOC(t, HAND(left, HAND(lu::leftUp, upUp, rightUp), right)) -> LOC(lu, HAND(List.append leftUp rightUp, upUp, [NODE (List.append left (t::right))]))
	| LOC(t, HAND(left, HAND([], upUp, []), right)) -> raise (NOMOVE "up of top")
	| LOC(t, HAND(left, TOP, right)) -> raise (NOMOVE "up of top")

let rec goDown loc =
	match loc with
	LOC(t, TOP) -> (match t with
					| NODE (h::etc) -> LOC(h, HAND([], TOP, etc))
					| _ -> raise (NOMOVE "down of bottom"))
	| LOC(t, HAND(left, up, right)) -> match t with
										LEAF leaf -> raise (NOMOVE "down of leaf")
										| NODE (h::etc) -> LOC(h, HAND([], HAND(left, up, right), etc))
										| _ -> raise (NOMOVE "down of bottom")