exception NotImplemented

  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec result : exp -> var
  = fun exp ->
      match exp with
      | V var -> var
      | P (v, e) -> result(e)
      | C (e1, e2) -> result(e2)

  let rec check : exp -> bool
  = fun exp -> 
      match exp with
      | V var -> false
      | P (v1, V v2) -> if v1 = v2 then true else false
      | P (v1, P (v2, e2)) -> if check(P (v2, e2)) || v1 = result(e2) then true else false
      | P (v1, C (e1, e2)) -> if v1 = result(e2) then true else false
      |_ -> raise NotImplemented;;
