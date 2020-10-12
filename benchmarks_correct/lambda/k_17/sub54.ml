  (**********************)
  (*   Problem 2        *)
  (**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
            and var = string

let rec check : lambda -> bool
= fun lam -> 
  let rec removevar : var -> (var list) -> (var list)
  = fun v l -> 
    match l with 
    | [] -> []
    | h::t -> if h = v then (removevar v t) else [h] @ (removevar v t)
  in
  let rec checklist : lambda ->  (var list)
  = fun lm ->
    match lm with 
    | V v -> [v]
    | P (v, l) -> (removevar v (checklist l))
    | C (l1, l2) -> ((checklist l1) @ (checklist l2))
  in
  match (checklist lam) with
  | [] -> true
  | _ -> false
