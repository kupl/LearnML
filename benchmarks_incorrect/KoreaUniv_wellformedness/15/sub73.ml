  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string
  
  let check : lambda -> bool
  =fun e -> true (* TODO *)
