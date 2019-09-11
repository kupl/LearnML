(*
	CSE / 2013-11426 / Im DongYeop
	Homework 2: Exercies 4
*)

type lambda = V of var
					 | P of var * lambda
					 | C of lambda * lambda
and var = string

let rec isitin((l: string list), (s: string)): bool = 
	match l with
	| [] -> false
	| _ -> (
		if List.mem s l == true
			then true
		else
			false)
(*	| hd::tl -> (
		if (hd = s)
			then false
		else
			isitin(tl, s))*)
(*		(
		if (List.hd l == s)
			then true
		else
			isitin(List.tl l, s))
*)
let rec inner((l: string list), (m: lambda)): bool =
	match m with
	| V s -> isitin(l, s)
	| P(nn, mm) -> inner(nn::l, mm)
	| C(c1, c2) -> (
		if inner(l, c1) == true && inner(l, c2) == true
			then true
		else
			false)

let check(m: lambda): bool =
	inner([], m)

