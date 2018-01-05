(* 2009-11824 Jieun-Jeong HW1-8 *)

type item = string
exception NOMOVE of string

type tree = LEAF of item
	| NODE of tree list

type zipper = TOP
	| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goRight loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "right of top")
	|LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	|LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let goUp loc = 
	let rec input_left llst lst = match llst with
		[]	-> lst
		|a::l	-> (input_left l (a::lst))
	in
	let make_UpTree tree llst rlst =
		NODE (((input_left llst []) @ [tree]) @ rlst)
	in
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "top of top")
	|LOC(t, HAND(left, up, right)) -> LOC((make_UpTree t left right), up)

let goDown loc = 
	match loc with
	LOC(LEAF str, z) -> raise (NOMOVE "child of leaf")
	|LOC(NODE(a::l), z) -> LOC(a, HAND([], z, l))
	|_ -> raise (NOMOVE "Invalid_argument")
(*
let goLeft loc = match loc with
	LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first") *)
