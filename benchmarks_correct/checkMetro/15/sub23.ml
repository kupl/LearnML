type lambda = V of var
	   | P of var * lambda
	   | C of lambda * lambda
and var = string

let areas : string list = []

let rec checkM :(lambda*string list)-> bool = fun (lambda,areas) ->
	match lambda with
	|V n -> List.mem n areas
	|P (n,m) -> let areas = n::areas in checkM(m,areas)
	|C (m1,m2) -> checkM(m1,areas) && checkM(m2,areas)

let rec check : lambda -> bool = fun lambda ->
	checkM(lambda,areas)
