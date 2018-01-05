(* C:\Documents and Settings\Administrator\¹ÙÅÁ È­¸é\pl_second_homework\HW4_Exercise4.ml *)

(*Exercise 4*)
type item = string
type tree = LEAF of item
	   | NODE of (tree list)

type zipper = TOP
	     | HAND of (tree list) * zipper * (tree list)

type location = LOC of tree * zipper

exception NOMOVE of string
;;
let rec goLeft loc = 
	match loc with
	 LOC (t, TOP) -> raise (NOMOVE "left of top")
	|LOC (t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	|LOC (t, HAND([],up,right)) -> raise (NOMOVE "left of first") 

let rec goRight loc = 
	match loc with
	 LOC (t, TOP) -> raise (NOMOVE "right of top")
	|LOC (t, HAND(left, up,r::right)) -> LOC(r, HAND(t::left, up, right))
	|LOC (t, HAND(left,up,[])) -> raise (NOMOVE "right of first") 

let rec goUp loc = 
	match loc with
	LOC (t, TOP) -> raise (NOMOVE "up of top")
	|LOC (t, HAND (left, up, right)) -> LOC ( NODE ( left@[t]@right), up)

let rec goDown loc =
	match loc with
	LOC (LEAF ele, h) -> raise (NOMOVE " down of LEAF")
	|LOC ( NODE(a::right),h) -> LOC (a, HAND([],h,right))
	|LOC (NODE [], _) -> raise (NOMOVE " EMPTY NODE")
;;

