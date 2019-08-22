
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let check : lambda -> bool
  = fun lambda -> (* TODO *)
  
  let rec checkvar lambda mylist=
  match lambda with 
  |V(var) -> if List.mem var mylist then true else false
  |P(var, mylambda) -> checkvar mylambda (mylist@[var])
  |C(lambda1,lambda2) -> (checkvar lambda1 mylist) && (checkvar lambda2 mylist)
	in
	checkvar lambda [] 
