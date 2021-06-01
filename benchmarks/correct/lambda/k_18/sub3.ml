type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec lookup_env : var -> var list -> bool
= fun x env ->
  match env with
  | [] -> false
  | y::tl -> if x = y then true else lookup_env x tl;;

let rec extend_env : var -> var list -> var list
= fun x env -> x :: env;;

let rec any_free : lambda -> var list -> bool
= fun lam env ->
  match lam with
  | V x -> lookup_env x env
  | P (x, l) ->
    let new_env = extend_env x env in
      any_free l new_env
  | C (l1, l2) -> (any_free l1 env) && (any_free l2 env)

let check : lambda -> bool
= fun lam -> any_free lam [];;
