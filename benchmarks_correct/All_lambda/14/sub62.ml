type lambda = V of var
| P of var*lambda
| C of lambda*lambda
and var = string

let rec check2 lambda lst =
  match lambda with
    | V(var) -> List.exists (fun x -> x=var) lst
    | C(a,b) -> (check2 a lst)&&(check2 b lst)
    | P(n,m) -> check2 m (n::lst)

let check lambda =
  check lambda []
