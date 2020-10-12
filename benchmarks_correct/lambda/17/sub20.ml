type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec check : lambda -> bool = fun met ->

	let rec lambdaList stList mtr =
		match mtr with
		|V var -> List.mem var stList
		|P (var,lambda) -> lambdaList (var::stList) lambda
		|C (met1,met2)-> (lambdaList stList met1) && (lambdaList stList met2)

	in lambdaList [] met


