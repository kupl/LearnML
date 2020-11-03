(* 2009-11824 Jieun-Jeong HW1-7 *)

type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let check input =
	let rec is_in_area n lst =
		match lst with
		[]	-> false
		|a::l	-> if a = n then true else (is_in_area n l)
	in
	let rec check2 m lst =
		match m with
		V n	-> (is_in_area n lst)
		|P (n, x)	-> (check2 x (n::lst)) 
		|C (l, r)	-> if (check2 l lst) then (check2 r lst) else false
	in
	check2 input []

