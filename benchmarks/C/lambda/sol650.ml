type lambda = 
  | V of var              (*variable*)
  | P of var * lambda     (*procedure abstraction*)
  | C of lambda * lambda  (*call*)
and var = string

type env = var list

let rec find : var -> env -> bool
= fun x env ->
  match env with 
    | y::tl -> if x = y then true else find x tl 
    | [] -> false;;

let rec solve : lambda -> env -> bool
= fun lam env ->
  match lam with 
    | V x -> find x env
    | P (x, l) -> solve l (x::env)
    | C (l1, l2) -> (solve l1 env) && (solve l2 env);;

let check : lambda -> bool
= fun lam -> solve lam [];;