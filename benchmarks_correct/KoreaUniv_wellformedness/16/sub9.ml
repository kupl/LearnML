
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec lst_check : string -> string list -> bool
  = fun var env ->
  match env with
  | [] -> false
  | hd::tl -> if hd = var then true else (lst_check var tl) 

  let rec well : exp -> string list -> bool
  = fun exp env ->
  match exp with
  | V var -> (lst_check var env)
  | P (var, e) -> (well e (var::env))
  | C (e1, e2) -> (well e1 env) && (well e2 env)

  let check : exp -> bool
  = fun exp -> (well exp [])
