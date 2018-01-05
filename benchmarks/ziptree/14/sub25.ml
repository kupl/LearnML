type item = string
type tree = LEAF of item
			| NODE of tree list
type zipper = TOP
			| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc =
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc =
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")


let goUp loc =
	let rec merge (l, m, r) =
		(* l, r : tree list
			m : tree
		return value : tree(== NODE of tree list) *)
		match l with
		| [] -> NODE(m::r)
		| fst::rst -> (merge (rst, fst, m::r))	
	in
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "up of TOP")
	| LOC(t, HAND(left, up, right)) -> 
		match up with
		| TOP -> LOC((merge (left, t, right)), TOP)
		| HAND(_left, _up, _right) -> LOC((merge (left, t, right)), HAND(_left, _up, _right))

let goDown loc =
(*	let rec merge (l, m, r) =
		

	in
*)	match loc with
	| LOC(LEAF a, _) -> raise (NOMOVE "down of LEAF")
	| LOC(NODE([]), _) -> raise (NOMOVE "down of bottom")
	| LOC(NODE(leftest::t), TOP)-> LOC(leftest, HAND([], TOP, t))
	| LOC(NODE(leftest::t), HAND(left, up, right)) -> LOC(leftest, HAND([], HAND(left, up, right), t))

