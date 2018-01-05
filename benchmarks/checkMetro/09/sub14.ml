(* 2007-11651 KIM DONG HYUN *)

type metro = STATION of name
						| AREA of name * metro
						| CONNECT of metro * metro
and name = string

let checkMetro metro =
	let rec getStn m =
		match m with
			STATION name -> [name]
		| AREA (n, m')
		-> List.filter (fun x->(String.compare x n)!=0) (getStn m')
		| CONNECT (m1, m2) -> (getStn m1) @ (getStn m2)
	in
	
	if (getStn metro) = [] then true
	else false;;