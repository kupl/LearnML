
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string
 
  let listP = [""]

  let rec check : exp -> bool
  = fun exp ->
 match exp with
 | V x -> List.mem x listP
 | P (x, e1) -> let listP = listP @ [x] in check e1 
 | C (e1, e2) -> check e1 && check e2
