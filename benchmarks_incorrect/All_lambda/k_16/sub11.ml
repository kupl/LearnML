
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec isexist : var list -> var -> bool
  = fun vars v ->
  match vars with
  | [] ->	false
  | hd::tl ->	if hd = v then true else isexist tl v

  let rec chkvars : lambda -> var list -> bool
  = fun lambda vars ->
  match lambda with
  | V v -> isexist vars v
  | P (v, e) ->	chkvars e (v::vars)
  | C (V v, e) ->	chkvars e (v::vars)

  let check : lambda -> bool
  = fun lambda -> 
  chkvars lambda []
