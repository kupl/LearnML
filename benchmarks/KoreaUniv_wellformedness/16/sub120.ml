
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec scan : var -> var list -> bool
  = fun s l ->
  match l with
  | [] -> false
  | hd::tl -> if hd = s then true
               else scan s tl

  let check : exp -> bool
  = fun exp -> 
    let rec subFun : exp * var list  -> bool
    = fun (e, env) ->
    match e with
    | V v -> if scan v env then true
             else false
    | P (v, e) -> subFun (e, v::env)
    | C (e1, e2) -> if subFun (e1, env) && subFun (e2, env) then true
                     else false
  in subFun (exp, [])
