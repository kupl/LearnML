type item = string
type tree =	LEAF of item
		  | NODE of tree list

type zipper = TOP
			| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

let goRight l = match l with
	  LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")
	| LOC(t, HAND(left, up, x::right)) -> LOC(x, HAND(t::left, up, right))

let goUp l = match l with
	  LOC(t, TOP) -> raise (NOMOVE "up of top")
	| LOC(t, HAND(left, u, right)) -> LOC(NODE( (List.rev left)@[t]@right ), u)

let goDown l = match l with
	  LOC(LEAF _, _) -> raise (NOMOVE "down of leaf")
	| LOC(NODE [], _) -> raise (NOMOVE "down of leaf?")
	| LOC(NODE (t::right), z) -> LOC(t, HAND([], z, right))

