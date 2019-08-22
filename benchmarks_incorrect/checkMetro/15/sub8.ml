type lambda = 	V of var
		| P of var * lambda
		| C of lambda * lambda
and var = string

let rec station_list x =
	match x with
	| V n -> [n]
	| P (n, m) -> (station_list m)
	| C (m1, m2) -> List.append (station_list m1) (station_list m2)

let rec check (x: lambda): bool =
	match x with
	| V n -> false
	| P (n, m) -> List.mem n (station_list m)
	| C (m1, m2) -> (check m1) && (check m2)
