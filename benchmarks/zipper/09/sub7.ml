exception Error of string

(* EX3 : zipper *)
let rec zipper( a, b ) =
	match a with
		[] -> b
		(* b where a is an empty list *)
		| h1 :: t1 ->
			match b with
			[] -> h1 :: t1
			(* a where b is an empty list *)
			| h2 :: t2 -> ( h1 :: h2 ::[] ) @ zipper( t1, t2 )
			(* normal case : append the list of heads of two lists
					 into the result of zipper with tails
					 of two lists *)

