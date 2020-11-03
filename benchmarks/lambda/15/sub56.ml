type lambda = V of var
		   	| P of var * lambda
		   	| C of lambda * lambda
and var = string

let rec myCheck m l = 
	match m with
	| V n -> (List.mem n l)
	| P (n, mm) -> if (List.mem n l) then (myCheck mm l) else (myCheck mm (n::l))
	| C (mm1, mm2) -> (myCheck mm1 l) && (myCheck mm2 l)

let rec check m =
	myCheck m []
