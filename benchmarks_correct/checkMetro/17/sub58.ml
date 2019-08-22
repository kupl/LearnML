(*Lee Seok Jin 2013-11417 CSE hw2_4*)

type lambda	= V of var
		| P of var * lambda
		| C of lambda * lambda

and var = string

(* using anonymous function would be better as we learned in PP
let compare((st, stan): 'a * 'a): bool = 
*)

let check(m: lambda) : bool = 
	let rec rec_check((met:lambda),(area_list:string list)): bool = 
		match met with 
		| V(s) -> List.exists(fun x -> x=s) area_list
		| P(s, _m) -> rec_check(_m, s::area_list)
		| C(l,r) -> rec_check(l, area_list) && rec_check(r,area_list)
	in rec_check(m, [])
