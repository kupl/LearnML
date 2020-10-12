type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let startlist = []

let rec check1 m listoflambda= 
  match m with
  | V var1 -> List.mem var1 listoflambda
  | P(n1, lambda) -> check1 lambda (n1::listoflambda)
  | C(lambda1, lambda2) -> (check1 lambda1 listoflambda)&&(check1 lambda2 listoflambda)

let check m = check1 m startlist
