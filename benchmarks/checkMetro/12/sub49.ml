type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let checkMetro = (fun x -> 
	let rec icm = (fun x l -> match x with
	| STATION n -> (List.mem n l)
	| CONNECT (a, b) -> (icm a l) && (icm b l)
	| AREA (n, a) -> (icm a (l@[n]))
		) in (icm x [])
);;