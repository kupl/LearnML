(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let extenv x env = x :: env
let rec search env lam =
match env with
|[] -> false
|a::tl -> if lam = a then true else search tl lam

let rec checklambda lambda env
= match lambda with
|V x -> search env x
|P(x, l) -> let newenv1 = extenv x env in
            checklambda l newenv1
|C(l1, l2) -> checklambda l1 env && checklambda l2 env

let rec check : lambda -> bool
= fun lam -> checklambda lam []
