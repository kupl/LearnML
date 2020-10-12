type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec in_env env var = 
  match env with
    |hd::tl -> if hd=var then true else in_env tl var
    |[] -> false

let rec check : lambda -> bool
= fun lam -> 
  let rec free env exp =
  (match exp with
    |V v -> in_env env v
    |P(v,l)-> free (v::env) l 
    |C(l1,l2)-> (free env l1) && (free env l2)) in free [] lam
    
