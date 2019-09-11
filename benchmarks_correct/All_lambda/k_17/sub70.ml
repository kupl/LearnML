(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> 
let rec find_var var env =
match env with
| [] -> []
| hd::tl -> if hd = var then find_var var tl else hd::(find_var var tl)
in
let rec eval lam =
match lam with
| V x -> [x]
| P(x,e) -> find_var x (eval e)
| C(e1,e2) -> (eval e1)@(eval e2)
in 
if (eval lam) = [] then true else false