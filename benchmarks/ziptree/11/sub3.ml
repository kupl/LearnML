(*2006-11681 °­Çö¼®*)
type item = string
exception NOMOVE of string

type tree = LEAF of item
		  | NODE of tree list

type zipper = TOP
			| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goRight loc =
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last")

let goUp loc =
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "up of top")
	| LOC(t, HAND(left, up, right)) -> 
		LOC(NODE((List.rev left)@([t]@right)),up)

let goDown loc =
	match loc with
	LOC(LEAF l, _) -> raise (NOMOVE "down of leaf")
	| LOC(NODE (h::t), z) -> LOC(h, HAND([],z,t)) 
	| LOC(NODE [], _) -> raise (NOMOVE "wrong tree")
(*
let l0 = LOC(LEAF "*",
			 HAND([LEAF "c"],
			 	  HAND([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]],
				  		TOP,
						[]),
				  [LEAF "d"]))
*)
