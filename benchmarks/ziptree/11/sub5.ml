(* hw 1_8. *)
type item = string
exception NOMOVE of string
type tree = LEAF of item
		  | NODE of tree list
type zipper = TOP
			| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goLeft loc =
	match loc with
	 LOC(t, TOP) -> raise (NOMOVE "left of top")
	|LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	|LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")
let goRight loc =
	match loc with
	 LOC(t, TOP) -> raise (NOMOVE "right of top")
	|LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	|LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")
let goUp loc =
	match loc with
	 LOC(t, TOP) -> raise (NOMOVE "up of top")
	|LOC(t, HAND(left, up, right)) -> LOC(NODE(List.rev left @ [t] @ right), up)
	|LOC(t, HAND(left, TOP, right)) -> raise (NOMOVE "up of top")
let goDown loc =
	match loc with
	 LOC(LEAF _, _) -> raise (NOMOVE "down of bottom")
	|LOC(NODE(t::tr), TOP) -> LOC(t, HAND([], TOP, tr))
	|LOC(NODE(t::tr), zip) -> LOC(t, HAND([], zip, tr))
	|LOC(NODE([]), _) -> raise (NOMOVE "??")

	(*
let l = LOC(LEAF "*", HAND([LEAF "c"], HAND([LEAF "+"; NODE[LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [LEAF "d"]))
	*)
