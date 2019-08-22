
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec chklst (lambda,lst) =
  match lambda with (* should be call by value *)
  | P(var,lambda) -> chklst(lambda, lst@[var])
  | C(lambda1,lambda2) -> if(chklst(lambda1, lst) && chklst(lambda2, lst)) then true else false
  | V var -> if lst=[] then false
    else List.exists (fun x -> (x=var)) lst

  let check : lambda -> bool
  = fun lambda -> chklst (lambda,[] )
