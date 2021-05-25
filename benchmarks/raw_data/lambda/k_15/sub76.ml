  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string
  
  let rec check : lambda -> bool
  =fun e -> 
    let rec remove_var lambda var =
    match lambda with
    | P (v,ex) -> P (v, remove_var ex var)
    | C (ex,ex') -> C (remove_var ex var, remove_var ex' var)
    | V v -> if (v = var) then V "c" else V v
    in

    match e with
    | P (v,ex) -> check (remove_var ex v)
    | C (ex,ex') -> check ex && check ex'
    | V v -> if v <> "c" then false else true
