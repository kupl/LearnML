(*Computer Science Engineering 2015-12683 Kim Jaein*)
type lambda = V of var
		   | P of var * lambda
		   | C of lambda * lambda
and var = string

let rec checkhelp (input, a_list) = 
	match input with
	|V a -> List.exists (fun x -> x = a) a_list
	|P (a, inside) -> checkhelp (inside, (List.append [a;] a_list))
	|C (a, b) -> checkhelp (a, a_list) && checkhelp (b, a_list)

let check (input:lambda) =
	checkhelp (input, [])

