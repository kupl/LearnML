type var = string

type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec isexist : var list -> var -> bool
  = fun vars v ->
  match vars with
  | [] ->	false
  | hd::tl ->	if hd = v then true else isexist tl v

  let rec chkvars : exp -> var list -> bool
  = fun exp vars ->
  match exp with
  | V v -> isexist vars v
  | P (v, e) ->	chkvars e (v::vars)
  | C (V v, e) ->	chkvars e (v::vars)

  let check : exp -> bool
  = fun exp -> 
  chkvars exp []