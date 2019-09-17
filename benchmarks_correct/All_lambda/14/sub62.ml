type lambda = V of var
| P of var*lambda
| C of lambda*lambda
and var = string

let rec check lambda lst =
  match lambda with
    | V(var) -> List.exists (fun x -> x=var) lst
    | C(a,b) -> (check a lst)&&(check b lst)
    | P(n,m) -> check m (n::lst)

let check lambda =
  check lambda []
