type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string

let rec my_check lambda check =
	match lambda with
	|V x -> List.mem x check
	|P (var, met) -> my_check met (check @ [var])
	|C (met1, met2) ->
		if (my_check met1 check)=true then my_check met2 check
		else false



let check m = my_check m []
