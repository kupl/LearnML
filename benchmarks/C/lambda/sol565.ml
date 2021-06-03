(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> true

let rec lambda ex l = 
match ex with 
|V(x) -> if List.mem x l then true else false
|P(x,e) -> lambda e(l@[x])
|C(e1, e2) -> (lambda e1 l) && (lambda e2 l)

let check ex =
lambda ex []
