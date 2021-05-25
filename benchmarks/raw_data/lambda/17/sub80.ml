type lambda =	 V of var
		|P of var * lambda
		|C of lambda * lambda
and var = string

let rec list_finder: 'a list * 'a -> bool = fun(list_, element_) ->
        (
        match list_ with
            [] -> false
            | hd::tail -> ( if(hd = element_) then (true) else (list_finder(tail,element_)))
        )

let rec check_helper: lambda * string list -> bool = fun(met,env) ->
(
          match met with
                V(st_var) -> list_finder(env,st_var)
                |P(ar_var,lambda_) -> check_helper(lambda_,ar_var::env )
                |C(lambda1_,lambda2_) -> check_helper(lambda1_,env) && check_helper(lambda2_,env)

)

let rec check: lambda -> bool = fun(met)
-> 
	(
		check_helper(met,[])
	)



