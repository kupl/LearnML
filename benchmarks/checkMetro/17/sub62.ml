(*
	CSE / 2013-11426 / Im DongYeop
	Homework 2: Exercies 4
*)

type metro = STATION of name
					 | AREA of name * metro
					 | CONNECT of metro * metro
and name = string

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
let rec inner((l: string list), (m: metro)): bool =
	match m with
	| STATION s -> isitin(l, s)
	| AREA(nn, mm) -> inner(nn::l, mm)
	| CONNECT(c1, c2) -> (
		if inner(l, c1) == true && inner(l, c2) == true
			then true
		else
			false)

let checkMetro(m: metro): bool =
	inner([], m)

