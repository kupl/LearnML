type item = string

type tree = LEAF of item
	| NODE of tree list

type zipper = TOP
	| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goRight loc =
	match loc with
	|LOC(t, TOP) -> raise (NOMOVE "right of top")
	|LOC(t, HAND(l, u, h::r)) -> LOC(r, HAND(l@t, u, r))
	|LOC(t, HAND(l, u, [])) -> raise(NOMOVE "right of the first")

let goUp loc =
	match loc with
	|LOC(t, TOP) -> raise (NOMOVE "up of top")
	|LOC(t, HAND(l, u, r)) -> LOC(l@t@r, u)

let goDown loc =
	match loc with
	|LOC([], h) -> LOC([], HAND([], h, []))
	|LOC(LEAF l, h) -> raise(NOMOVE "no down")
	|LOC(h::t, h) -> LOC(h, HAND([], h, t))
	