  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec check : exp -> bool
  =fun e -> 
    let rec remove_var exp var =
    match exp with
    | P (v,ex) -> P (v, remove_var ex var)
    | C (ex,ex') -> C (remove_var ex var, remove_var ex' var)
    | V v -> if (v = var) then V "" else V v
    in

    match e with
    | P (v,ex) -> check (remove_var ex v)
    | C (ex,ex') -> check ex && check ex'
    | V v -> if v<> "" then false else true
