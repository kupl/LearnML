(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda

let rec check : lambda -> bool
= fun lam -> true (* TODO *)
