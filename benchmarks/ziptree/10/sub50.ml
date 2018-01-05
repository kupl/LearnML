(* C:\Users\owner\Desktop\Homework 1(8).ml *)

type item = string
and tree = LEAF of item
| NODE of tree list
and zipper = TOP
| HAND of tree list * zipper * tree list
and location = LOC of tree * zipper ;;

exception NOMOVE of string;;

let goLeft loc =
	match loc with
	LOC(NODE[], z) -> raise (NOMOVE "NULL tree!")
	| LOC(t, TOP) -> raise (NOMOVE "left of top")
	| LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first") ;;


let goRight loc =
	match loc with
	LOC(NODE[], z) -> raise (NOMOVE "NULL tree!")
	| LOC(t, TOP) -> raise (NOMOVE "right of top")
	| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
	| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of last") ;;


let goUp loc =
	match loc with
	LOC(NODE[], z) -> raise (NOMOVE "NULL tree!")
	| LOC(t, TOP) -> raise (NOMOVE "upward of top")
	| LOC(LEAF item, HAND(left, up, right)) -> LOC(NODE((List.rev left)@[LEAF item]@right), up)
	| LOC(NODE node, HAND(left, up, right)) -> LOC(NODE((List.rev left)@node@right), up) ;;

let goDown loc =
	match loc with
	LOC(NODE[], z) -> raise (NOMOVE "NULL tree!")
	| LOC(LEAF item, up) -> raise (NOMOVE "downward of leaf")
	| LOC(NODE(left::rest), up) -> LOC(left, HAND([], up, rest)) ;; 

