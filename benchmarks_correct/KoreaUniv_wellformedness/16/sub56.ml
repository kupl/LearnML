
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec exist : var*var list -> bool
  = fun (var,lst) -> match lst with
  | [] -> false
  | hd::tl -> if hd=var then true else exist(var,tl)

  let rec checkk : exp*var list -> bool
  = fun (exp,lst) -> match exp with
  | V(v) -> if(exist(v,lst)) then true else false
  | C(e1,e2) -> if(checkk(e1,lst) && checkk(e2,lst)) then true else false
  | P(v,e) -> checkk(e,v::lst)

  let rec check : exp -> bool
  = fun exp -> match exp with
  | V(v) -> false
  | C(e1,e2) -> if(check(e1) && check(e2)) then true else false
  | P(v,e) -> checkk(e,[v])
