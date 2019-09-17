	
type lambda = V of var
		| P of var * lambda  (*procedure*)
		| C of lambda * lambda  (*call lambdaression(procedure)*)
and var = string

let rec check lambda =
			match lambda with
				V var -> false
				|_->checkExp(lambda,[])


and checkExp = fun(lambda,env)->
		match lambda with
			V var-> (match env with 	
				[]-> false
				|hd::tail-> if hd = var then true else checkExp (lambda,tail)
				)
			|P (var,lambda)-> 	checkExp(lambda,(var::env))
			
			|C (lambda1,lambda2)->  
				if checkExp(lambda1,env) = true then 
				if checkExp(lambda2,env) = true then true else false else false
