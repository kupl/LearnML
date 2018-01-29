
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec check3 v l= match l with
    [] -> false
    |hd::tl -> if hd=v then true else check3 v tl

  let rec check2 e l = match e with
     V(v)->check3 v l
    |P(v, exp1)->check2 exp1 (l@[v])
    |C(exp2, exp3)->check2 exp2 l&&check2 exp3 l

  let check : exp -> bool
  = fun exp -> check2 exp []
