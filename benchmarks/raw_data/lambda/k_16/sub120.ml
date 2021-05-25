
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec scan : var -> var list -> bool
  = fun s l ->
  match l with
  | [] -> false
  | hd::tl -> if hd = s then true
               else scan s tl

  let check : lambda -> bool
  = fun lambda -> 
    let rec subFun : lambda * var list  -> bool
    = fun (e, env) ->
    match e with
    | V v -> if scan v env then true
             else false
    | P (v, e) -> subFun (e, v::env)
    | C (e1, e2) -> if subFun (e1, env) && subFun (e2, env) then true
                     else false
  in subFun (lambda, [])
