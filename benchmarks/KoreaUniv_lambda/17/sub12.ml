(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let empty = []

let rec check : lambda -> bool
= fun lam -> 
  let rec eval l env =
    match l with
    |V(x) -> (match env with
            |[] -> false
            |hd::tl -> if hd=x then true else eval l tl)
    |P(x, l1) -> eval l1 ([x]@env)
    |C(l1, l2) -> (eval l1 env) && (eval l2 env)
  
  in
  eval lam empty




