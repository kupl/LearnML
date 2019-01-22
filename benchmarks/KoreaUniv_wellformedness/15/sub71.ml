	
type exp = V of var
		| P of var * exp  (*procedure*)
		| C of exp * exp  (*call expression(procedure)*)
and var = string

let rec check exp =
			match exp with
				V var -> false
				|_->checkExp(exp,[])


and checkExp = fun(exp,env)->
		match exp with
			V var-> (match env with 	
				[]-> false
				|hd::tail-> if hd = var then true else checkExp (exp,tail)
				)
			|P (var,exp)-> 	checkExp(exp,(var::env))
			
			|C (exp1,exp2)->  
				if checkExp(exp1,env) = true then 
				if checkExp(exp2,env) = true then true else false else false
