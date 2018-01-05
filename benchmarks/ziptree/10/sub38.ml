exception NOMOVE of string
exception Error of string

type tree = LEAF of item
	  | NODE of tree list

and item = string ;;

type zipper = TOP
	    | HAND of tree list * zipper * tree list;;

type location = LOC of tree * zipper;;



let rec append_list a b c=
	match (a,b) with
	(h::t,k) -> h::(append_list t b c)
	|([],h::t) -> h::(append_list a t c)
	|([],[]) -> c ;;


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
	|LOC(t, HAND(l,up,r)) -> LOC((NODE (append_list (List.rev l) [t] r)), up)
					


	
let rec goDown loc =
	match loc with
	LOC(LEAF a, k) -> raise (NOMOVE "left of bottom")
	|LOC(NODE (h::t), hand) -> LOC(h, HAND([],hand,t))   
	|LOC(NODE [],k) -> raise (NOMOVE "error")	



