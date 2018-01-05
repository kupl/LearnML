exception NOMOVE of string

type tree = LEAF of item
	  | NODE of tree list

and item = string ;;

type zipper = TOP
	    | HAND of tree list * zipper * tree list;;

type location = LOC of tree * zipper;;


let rec goLeft loc = 
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "left of top")
	|LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	|LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first");;

let rec goRight loc =
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "left of top")
	|LOC(t, HAND(left,up,r::right)) -> LOC(r, HAND(t::left, up, right))
	|LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first");;

let rec goUp loc =
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "left of top")
	|LOC(t, HAND(left,TOP,right)) -> raise (NOMOVE "left of top")
	|LOC(t, HAND(left,up,right)) -> raise (NOMOVE "left of top");;

	(*
let rec goDown loc =
	match loc with
	LOC(t, TOP) -> raise (NOMOVE "left of top")
	|LOC(t, HAND(left, up, right)) -> LOC(left, HAND([], TOP, t::right));;
	*)
