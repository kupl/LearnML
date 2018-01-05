exception NOMOVE of string

type item = string
type tree = LEAF of item
			| NODE of tree list

type zipper = TOP
			| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc =
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of the first")

let goRight loc = 
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of the last")

let goUp loc = 
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "up of top")
	| LOC(t, HAND(left, up, right)) -> 
		let self = NODE ((List.rev left)@(t::right)) in
		match up with
		| TOP -> LOC(self, TOP)
		| HAND (ls, upper, rs) -> LOC(self, upper)
	
let goDown loc = 
	match loc with LOC(t, zip) -> (
		match t with 
		| LEAF i -> raise (NOMOVE "down of bottom")
		| NODE [] -> raise (NOMOVE "down of empty node")
		| NODE (hd::tl) -> LOC(hd, HAND([], zip, tl))
	)