(* 2009-11824 Jieun-Jeong HW1-7 *)

type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let checkMetro input =
	let rec is_in_area n lst =
		match lst with
		[]	-> false
		|a::l	-> if a = n then true else (is_in_area n l)
	in
	let rec check m lst =
		match m with
		STATION n	-> (is_in_area n lst)
		|AREA (n, x)	-> (check x (n::lst)) 
		|CONNECT (l, r)	-> if (check l lst) then (check r lst) else false
	in
	check input []

