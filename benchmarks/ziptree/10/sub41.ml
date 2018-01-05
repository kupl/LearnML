(* 4190.310 Programming Language			*
 * Homework #1 - Exercise 8 (짚-짚-나무)	*
 * 2008-11744 Jongwook Choi 				*)

type item = string

type tree = LEAF of item
		  | NODE of tree list

type zipper = TOP
			| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper 


let testloc = LOC(LEAF "*", HAND([LEAF "c"],
	HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []),
		[LEAF "d"]));;


(* HERE BEGINS *)

exception NOMOVE of string

let goLeft loc = match loc with
	  LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([], _, _)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
	  LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(_, _, [])) -> raise (NOMOVE "right of last")

let goUp loc = match loc with
	  LOC(t, TOP) -> raise (NOMOVE "top of top")
	| LOC(t, HAND(_, TOP, _)) -> raise (NOMOVE "here is topmost")
	| LOC(t, HAND(left, HAND(l, u, r), right)) -> LOC( NODE(left @ [t] @ right), HAND(l, u, r) )

let goDown loc = match loc with
	  LOC(LEAF _, _) -> raise (NOMOVE "down of leaf")
	| LOC(NODE [], _) -> raise (NOMOVE "down of leaf?")
	| LOC(NODE (d::down), z) -> LOC(d, HAND([], z, down))

