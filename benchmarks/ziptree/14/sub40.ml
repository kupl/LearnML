type item = string

type tree = LEAF of item
		  | NODE of tree list

type zipper = TOP
			| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string


let goLeft loc = 
	match loc with
	| LOC (t, TOP) -> raise (NOMOVE "left of top")
	| LOC (t, HAND (l::left, up, right)) -> LOC (l, HAND (left, up, t::right))
	| LOC (t, HAND ([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc = 
	match loc with
	| LOC (t, TOP) -> raise (NOMOVE "right of top")
	| LOC (t, HAND (left, up, r::right)) -> LOC (r, HAND ([t]@left, up, right))
	| LOC (t, HAND (left, up, [])) -> raise (NOMOVE "right of first")

let goUp loc = 
	match loc with
	| LOC (t, TOP) -> raise (NOMOVE "top of top")
	| LOC (t, HAND (left, TOP, right)) -> LOC (NODE (left@[t]@right), TOP)
	| LOC (t, HAND (left, HAND (l, z, r), right)) -> LOC (NODE (left@[t]@right), HAND (l, z, r))

let goDown loc = 
	match loc with
	| LOC (LEAF i, TOP) -> raise (NOMOVE "leaf, no more down")
	| LOC (LEAF i, HAND (l, z, r)) -> raise (NOMOVE "bottom of first")
	| LOC (NODE (hd::tl), z) -> LOC (hd, HAND ([], z, tl))
	| LOC (NODE [], _) -> raise (NOMOVE "empty Node")