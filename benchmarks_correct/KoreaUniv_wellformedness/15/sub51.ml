  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string
  
  let check : lambda -> bool
  =fun e ->
    let rec helpCheck lambda var_list = 
    match lambda with
      | V var -> List.mem var var_list
      | P (var, e1) -> helpCheck e1 (var::var_list)
      | C (e1, e2) -> helpCheck e1 var_list && helpCheck e2 var_list
    in helpCheck e [] ;;
