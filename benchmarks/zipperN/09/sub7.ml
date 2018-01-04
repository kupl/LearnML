exception Error of string

(* EX4 : zipperN *)
let rec zipperN a =
	(* function rmEmpty : remove empty lists in a list of lists *)
	let rec rmEmpty a  =
		if a = [] then []
		else List.filter ( fun x -> x != [] ) a in
	(* function getHead : make a list which is a collection of heads of elements *)
	let rec getHead a =
		match a with
			[] -> []
			| hd :: tl -> List.hd hd :: getHead tl in
	(* function getTail : make a list that has lists whose head is moved as an element *)
	let rec getTail a =
		match a with
			[] -> []
			| hd :: tl -> List.tl hd :: getTail tl in
	if a = [] then []
	(* base case : empty list *)
	else getHead ( rmEmpty a ) @  zipperN ( getTail ( rmEmpty a ) )
