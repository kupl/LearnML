type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var=string
let rec check2(lst,lambda) =
    match lambda with
      V(var) -> List.mem var lst
    | P(var,m1) -> check2(var::lst,m1)
    | C(m1,m2) -> check2(lst,m1) && check2(lst,m2)
let check lambda = check2([],lambda)
