type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string


let rec matchName (n: var) (l: var list): bool =
	match l with
	| [] -> false
	| h::t -> if (n = h) then true
			  else (matchName n t)

let rec checkRec (m: lambda) (areas: var list): bool =
	match m with
	| V n -> (matchName n areas)
	| C (m1, m2) -> 
			((checkRec m1 areas) && (checkRec m2 areas))
	| P (n, m) -> (checkRec m (n::areas))

let check (m: lambda): bool =
	(checkRec m [])
