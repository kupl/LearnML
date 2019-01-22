
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let check : exp -> bool
  = fun exp -> (* TODO *)
  
  let rec checkvar exp mylist=
  match exp with 
  |V(var) -> if List.mem var mylist then true else false
  |P(var, myexp) -> checkvar myexp (mylist@[var])
  |C(exp1,exp2) -> (checkvar exp1 mylist) && (checkvar exp2 mylist)
	in
	checkvar exp [] 
