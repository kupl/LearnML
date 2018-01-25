  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let check : exp -> bool
  =fun e ->
    let rec helpCheck exp var_list = 
    match exp with
      | V var -> List.mem var var_list
      | P (var, e1) -> helpCheck e1 (var::var_list)
      | C (e1, e2) -> helpCheck e1 var_list && helpCheck e2 var_list
    in helpCheck e [] ;;
