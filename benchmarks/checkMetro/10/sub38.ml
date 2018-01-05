(* complete *)
type metro = STATION of name
	|AREA of name * metro
	|CONNECT of metro * metro
and name = string

let checkMetro met =
	let rec search n l = match l with
		[] -> false
		|h::t -> if h = n then true
				else search n t
	in
	let rec check m lst = match m with
		STATION n -> (search n lst)
		|AREA (a,b) -> check b (a::lst)
		|CONNECT (a,b) -> (check a lst)&&(check b lst)
	in
	check met []
;;
