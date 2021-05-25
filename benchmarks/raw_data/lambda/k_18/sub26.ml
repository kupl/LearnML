type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

type env = (var * lambda) list

let rec scan : env -> var -> bool
= fun env var ->
  match env with
  | [] -> false
  | (v, l)::tl -> if v = var then true else scan tl var
  
let rec check_ : env -> lambda -> bool
= fun env lam ->
  match lam with
  | V (v) -> scan env v
  | P (v, l) -> let env = (v, l)::env in check_ env l
  | C (l1, l2) -> check_ env l1 && check_ env l2

let check : lambda -> bool
= fun lam -> check_ [] lam;;