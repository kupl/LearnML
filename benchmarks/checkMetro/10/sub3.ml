(* CSE/ 2004-11920 / Yeseong Kim/ Prob 7*)

type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let checkMetro m =
		let rec subMetro myM l =
			match myM with
				STATION(n) -> (List.mem n l)
			|	AREA(n, subm) -> (subMetro subm (n::l))
			|	CONNECT(m1, m2) -> ((subMetro m1 l) && (subMetro m2 l))
		in
		(subMetro m [])
