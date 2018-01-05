(*2009-11718 1-9*)
exception NOMOVE of string

type item = string
type tree = LEAF of item
		| NODE of tree list

type zipper = TOP
			| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc = match loc with
	| LOC (t, TOP) -> raise (NOMOVE "left of top")
	| LOC (t, HAND(l::left, up, right)) -> LOC (l, HAND (left, up, t::right))
	| LOC (t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
	| LOC (t, TOP) -> raise (NOMOVE "right of top")
	| LOC (t, HAND(left, up, r::right)) -> LOC (r, HAND (t::left, up, right))
	| LOC (t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp loc = match loc with
	| LOC (t, TOP) -> raise (NOMOVE "it's top")
	| LOC (t, HAND(left, HAND(upLeft, up, upRight), right))->
		LOC (NODE ((List.rev left)@[t]@right), HAND(upLeft, up, upRight))
	| LOC (t, HAND(left, TOP, right))->
		LOC (NODE ((List.rev left)@[t]@right), TOP)

let goDown loc = match loc with
	| LOC (LEAF item, _) -> raise (NOMOVE "it's leaf")
	| LOC (NODE (downLeft::tl), HAND(left, up, right)) ->
		LOC (downLeft, HAND([], HAND(left, up, right), tl))
	| LOC (NODE (downLeft::tl), TOP) ->
		LOC (downLeft, HAND([], TOP, tl))
	| LOC (NODE _, _) -> raise (NOMOVE "it's bottom")
