type lambda = 
	|V of var
	|P of var * lambda
	|C of lambda * lambda
and var = string

let rec checking met env =
	match met with
	| V st -> List.mem st env
	| P(ar,m) ->	checking m (ar::env)
	| C(m1,m2) ->	(checking m1 env) && (checking m2 env)

let rec check met =
	(checking met [])
	

	