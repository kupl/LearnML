
(* ex 8 *) 

type item = string

type tree = LEAF of item
		  | NODE of tree list

type zipper = TOP
			| HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string

(* ---------------------------------------------------------- *)

let make_node(l1,t,l2) = NODE (l1 @ (t::l2))

(*
let tree_car tr = 
	match tr with
	| NODE h::t -> h

let tree_cdr tr =
	match tr with
	| NODE h::t -> NODE t
*)


(* ---------------------------------------------------------- *)

let goLeft loc = 
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))

let goRight loc =
	match loc with
	| LOC(t, TOP) -> raise (Invalid_argument "right of top")
	| LOC(t, HAND(left,up,[])) -> raise (Invalid_argument "right of last")
	| LOC(t, HAND(left,up,r::right)) -> LOC(r, HAND(t::left,up,right))

let goUp loc = 
	match loc with 
	| LOC(t, TOP) -> raise (Invalid_argument "already on the top")
	| LOC(t, HAND(left,up,right)) -> LOC(make_node(left,t,right),up)

let goDown loc =
	match loc with
	| LOC(LEAF _,_) -> raise (Invalid_argument "already on the bottom")
	| LOC(NODE tl,zipper) -> match tl with
							 | [] -> raise (Invalid_argument "tree is wrong!") 
							 | h::t -> LOC(h, HAND([],zipper,t))


