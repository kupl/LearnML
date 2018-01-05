(* 2009-11674 ±è¿øÁø HW1-8 *)

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
		| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
		| LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc =
	match loc with
		LOC(t, TOP) -> raise (NOMOVE "right of top")
		| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND((t::left), up, right))
		| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let goDown loc =
	match loc with
		| LOC(t, up) -> (match t with
					| LEAF a -> raise (NOMOVE "it is LEAF")
					| NODE [] -> raise (NOMOVE "empty NODE")
					| NODE (a::tr) -> LOC(a, HAND([],up,tr)))

let goUp loc =
	match loc with
		| LOC(t, up) -> (match up with
					| TOP -> raise (NOMOVE "top of top")
					| HAND([], u, r) -> LOC(NODE (t::r),u)
					| HAND(l, u, []) -> LOC(NODE ((List.rev(t::(List.rev l)))),u)
					| HAND(l, u, r) -> LOC(NODE ((List.append l (t::r))),u))
