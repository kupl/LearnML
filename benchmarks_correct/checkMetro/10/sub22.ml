(* Exercise 7 *)
type lambda =
	V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let rec checkId (lambda, env) =
	match lambda with
		V id ->
			(List.exists (fun x -> (x = id)) env)
		| P (id, sublambda) ->
			(checkId (sublambda, (id :: env)))
		| C (sublambda1, sublambda2) ->
			((checkId (sublambda1, env)) && (checkId (sublambda2, env)))

let rec check lambda =
	checkId (lambda, [])
