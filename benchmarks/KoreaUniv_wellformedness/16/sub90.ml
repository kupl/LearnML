
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec check : exp -> bool
  = fun exp -> 
  match exp with
  | V vari -> true
  | P (vari, exp) ->
    (match exp with
    |V v -> (if vari = v then true else false)
    |C (e1,e2) -> (check (P (vari,e1))||check (P (vari,e2)))&&check e1&&check e2
    |P (v,e) -> check (P (v,e)) && check (P (vari, e))
    )
  | C (exp1, exp2) -> check exp1 && check exp2