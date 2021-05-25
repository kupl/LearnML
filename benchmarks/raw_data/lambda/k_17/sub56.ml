(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> 

  let rec pop_value : var list -> var -> var list
  = fun l v->
    match l with
    | [] -> []
    | hd::tl -> if hd=v then (pop_value tl v) else hd::(pop_value tl v)
  in

  let rec free_value : lambda -> var list
  = fun l->
    match l with
    | V(v) -> [v]
    | P(v0, l0) -> pop_value (free_value l0) v0
    | C(l1, l2) -> (free_value l1)@(free_value l2)
  in

  (free_value lam)==[]

