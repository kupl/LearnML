type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let checkMetro : metro -> bool = fun met ->
	let rec helper : metro * string list -> bool = fun (m,l) ->
		match m with
		| STATION n -> List.mem n l
		| AREA (n,m) -> helper(m,n::l)
		| CONNECT (m1,m2) -> helper(m1,l) && helper(m2,l)

	in
	helper(met,[])
		
