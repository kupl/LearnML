
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec expToVar : exp -> var
  = fun exp ->
      match exp with
      |V var        -> var
      |P (var,ex)   -> expToVar(ex)
      |C (ex1,ex2)  -> expToVar(ex2)

  let rec check : exp -> bool
  = fun exp -> 
    let rec pcheck : exp * (var list) -> bool
    = fun (aexp, vlist) -> 
      match aexp with
        V s -> (match vlist with
          [] -> false
          | h::t -> if s = h then true else pcheck (V s, t))
      | P (v,exp) -> pcheck (exp, v::vlist)
      | C (exp1, exp2) -> (pcheck (exp1,vlist)) && (pcheck (exp2,vlist))            
    in 
  pcheck (exp,[]);;
