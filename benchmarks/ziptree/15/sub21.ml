type item = string
type tree =	LEAF of item
			| NODE of tree list
type zipper = TOP
			| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper
exception NOMOVE of string

let goRight loc = match loc with
	LOC(t, TOP)	-> raise (NOMOVE "right of top")
	|LOC(t, HAND(left, up, r::right))	->LOC(r, HAND(left@[t], up, right))
	|LOC(t, HAND(left, up, []))	-> raise (NOMOVE "right of last")

let goUp loc = match loc with
	LOC(t, TOP)	-> raise (NOMOVE "up of top")
	|LOC(t, HAND(left, HAND(up_left, up_up, up_right), right))
		-> LOC(NODE((List.rev left)@(t::right)), HAND(up_left, up_up, up_right))
	|LOC(t, HAND(left, TOP, right))	-> LOC(NODE((List.rev left)@(t::right)), TOP)

let goDown loc = match loc with
	LOC(t, z)	-> match t with
					LEAF(_)	-> raise (NOMOVE "down of leaf")
					|NODE(tli)	-> LOC(List.hd tli, HAND([], z, List.tl tli))
