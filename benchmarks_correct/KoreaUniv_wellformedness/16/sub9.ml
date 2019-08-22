
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec lst_check : string -> string list -> bool
  = fun var env ->
  match env with
  | [] -> false
  | hd::tl -> if hd = var then true else (lst_check var tl) 

  let rec well : lambda -> string list -> bool
  = fun lambda env ->
  match lambda with
  | V var -> (lst_check var env)
  | P (var, e) -> (well e (var::env))
  | C (e1, e2) -> (well e1 env) && (well e2 env)

  let check : lambda -> bool
  = fun lambda -> (well lambda [])
