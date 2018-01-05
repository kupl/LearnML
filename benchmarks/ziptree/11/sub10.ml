(* HW 1-8 / 2007-11603 / 컴퓨터공학부 / 이영준 *)

type item = string
exception NOMOVE of string 

type tree = LEAF of item
	  | NODE of tree list

type zipper = TOP
	    | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper	



let goLeft loc = match loc with
		LOC(t, TOP) -> raise (NOMOVE "left of top")
		| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
		| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
		LOC(t, TOP) -> raise (NOMOVE "right of top")
		| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
		| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

let goUp loc = match loc with
		LOC(t, TOP) -> raise (NOMOVE "Now, zipper is TOP")
		| LOC(t, HAND(left, TOP, right)) ->
			LOC (NODE(List.append (List.rev left) (t::right)), TOP)
		| LOC(t, HAND(left, HAND(l,z,r), right)) -> 
			LOC (NODE(List.append (List.rev left) (t::right)), HAND(l, z, r))

let goDown loc = match loc with
		LOC(LEAF (i), h) -> raise (NOMOVE "Now, zipper is LEAR")
		| LOC(NODE [], h) -> raise (NOMOVE "NOW, zipper is NODE []")
		| LOC(NODE (l::t), TOP) -> LOC(l, HAND([], TOP, t))
		| LOC(NODE (l::t), HAND(left,up,right))
			-> LOC(l, HAND([], HAND(left,up,right), t))