(* 2009-11824 Jieun-Jeong HW2-3 *)

type expr = NUM of int
		| PLUS of expr * expr
		| MINUS of expr * expr
		| MULT of expr * expr
		| DIVIDE of expr * expr
		| MAX of expr list

let rec eval expr = 
	let rec find_max lst max =
		match lst with
		|[]		-> max
		|a::l	-> if (eval a) > max then (find_max l (eval a))
					else (find_max l max)
	in
	match expr with 
	|NUM num			-> num
	|PLUS (el, er)		-> ((eval el) + (eval er))
	|MINUS (el, er)		-> ((eval el) - (eval er))
	|MULT (el, er)		-> ((eval el) * (eval er))
	|DIVIDE (el, er)	-> ((eval el) / (eval er))
	|MAX lst			-> (find_max lst 0)
