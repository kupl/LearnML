type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let checkMetro metro =
	let rec isThereID metro l =
		match metro with
		| STATION n -> List.mem n l
		| AREA (n, m) -> isThereID m (n::l)
		| CONNECT (m1, m2) -> (isThereID m1 l && isThereID m2 l)
	in
	isThereID metro []