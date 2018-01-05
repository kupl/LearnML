type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string


let checkMetro t =
	let rec getid t r = 
	match t with
	AREA(t1, t2) -> getid t2 (t1::r)
	|CONNECT(t1, t2) -> (getid t1 r) && (getid t2 r)
	|STATION t1 -> List.exists (fun x -> if x = t1 then true else false) r
	in

	getid t []
