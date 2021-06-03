(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec find lst var =
match lst with
| [] -> false
| hd::tl -> if hd = var then true else find tl var

let rec compare varlist lambda =
match lambda with
| V x -> find varlist x
| P(x, l) -> compare (varlist@[x]) l
| C(l1, l2) -> compare varlist l1 && compare varlist l2

let rec check : lambda -> bool
= fun lam -> 
match lam with
| V x -> false
| P(x, l) -> compare [x] l
| C(l1, l2) -> check l1 && check l2


