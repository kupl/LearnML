(* 2008-11874 Lee, Sujee *)
(* EXERCISE 8 *)

type item = string 
exception NOMOVE of string 

type tree = LEAF of item
| NODE of tree list
type zipper = TOP
| HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

let goLeft loc = (* goLeft : location -> location = <fun> *)
	match loc with
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

let goRight loc = (* goRight : location -> location = <fun> *)
	match loc with
		| LOC(t,TOP) -> raise (NOMOVE "right of top")
		| LOC(t, HAND(left,up,r::right)) -> LOC(r,HAND(t::left,up,right))
		| LOC(t, HAND(left,up,[])) -> raise (NOMOVE "right of last")

let goUp loc = (* goUp : location -> location = <fun> *)
	match loc with
		| LOC(t,TOP) -> raise (NOMOVE "up of top")
		| LOC(t, HAND(left,up,right)) -> LOC(NODE(List.append (List.rev left) (t::right)),up)
		(* left is reverse list of left siblings, therefore should reverse it. *)
		
let goDown loc = (* goDown : location -> location = <fun> *)
	match loc with
		| LOC(NODE(mostleft::children), hand) -> LOC(mostleft, HAND([],hand,children))
		| LOC(LEAF l, hand) -> raise (NOMOVE "down of leaf")
		| LOC(NODE([]),_) -> raise (NOMOVE "empty node")

