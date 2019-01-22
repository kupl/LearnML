type exp = V of var
         | P of var * exp
         | C of exp * exp
and var = string

let addlist : exp -> var list-> var list
=fun e l ->
match e with
  |P (v,e1) -> v::l
  |_ -> l

let rec check_env : var list-> var -> bool
=fun l v ->
match l with
  |[] -> false
  |hd::tl -> if (hd=v) then true
  else (check_env tl v)

let var_list : var list = []

let rec eval : exp -> var list -> bool
=fun e l ->
match e with
  |V var -> check_env l var
  |P (v,e1) ->(eval e1 (addlist e l))
  |C (e1,e2) -> (eval e1 l) && (eval e2 l)

let rec check : exp -> bool
=fun e -> eval e var_list
