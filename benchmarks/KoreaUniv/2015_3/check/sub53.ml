  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec check_list(exp, lst) = 
	match exp with
	|V(var) -> if List.mem var lst then true else false
	|P(var,exp) -> check_list(exp, lst@[var])
	|C(exp1, exp2) -> check_list(exp2,lst) && check_list(exp2,lst)

  let check : exp -> bool
  =fun e -> check_list(e,[])
