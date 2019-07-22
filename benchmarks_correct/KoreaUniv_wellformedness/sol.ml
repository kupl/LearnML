type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
and var = string

let rec is_mem : var list -> var -> bool
= fun variables var ->
  match variables with
  | [] -> false
  | hd::tl -> if (hd = var) then true else is_mem tl var

let rec sub_check : exp -> var list -> bool
= fun exp vars ->
  match exp with
  | V x -> is_mem vars x
  | P (x, e) -> sub_check e (x::vars)
  | C (e1, e2) -> (sub_check e1 vars) && (sub_check e2 vars)

let rec check : exp -> bool
= fun exp ->
  sub_check exp []
