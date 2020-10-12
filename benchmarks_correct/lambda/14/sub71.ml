type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string;; 

let rec checkG lst met =
	match met with
	| P(n, m) -> checkG (n::lst) m
	| C(m1, m2) -> checkG lst m1 && checkG lst m2
	| V t ->
		if List.mem t lst then true
		else false;;

let check l = checkG [] l;;	