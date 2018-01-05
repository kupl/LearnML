(* 컴퓨터공학부/2009-11679/김정명/8 *)

type item = string
exception NOMOVE of string
type tree = LEAF of item
		  | NODE of tree list
type zipper = TOP
			| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goLeft loc =
	match loc with
	  LOC (t, TOP) -> raise (NOMOVE "left of top")
	| LOC (t, HAND (l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC (t, HAND ([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc =
	match loc with
	  LOC (t, TOP) -> raise (NOMOVE "right of top")
	| LOC (t, HAND (left, up, l::right)) -> LOC(l, HAND(left @ [t], up, right))
	| LOC (t, HAND (left, up, [])) -> raise (NOMOVE "right of first")
;;

let goUp loc =
	match loc with
	  LOC (t, TOP) -> raise (NOMOVE "top")
	| LOC (LEAF l, HAND (left, up, right)) -> LOC(NODE (left @ [LEAF l] @ right), up)
	| LOC (NODE n, HAND (left, up, right)) -> LOC(NODE (left @ n @ right), up)
;;

let goDown loc =
	let rec getChildren treelist =
		match treelist with
		  [] -> []
		| (LEAF l)::t -> getChildren t
		| (NODE n)::t -> n @ (getChildren t)
	in
	match loc with
	  LOC (LEAF l, _) -> raise (NOMOVE "bottom")
	| LOC (NODE [], _) -> raise (NOMOVE "bottom")
	| LOC (NODE (h::t), TOP) -> LOC (h, HAND ([], TOP, t))
	| LOC (NODE (h::t), HAND (left, up, right)) -> LOC (h, HAND ([], HAND (left, up, right), t))
;;
