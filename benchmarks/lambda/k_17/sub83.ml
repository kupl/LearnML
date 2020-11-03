(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> 
let rec checklam e l = 
match l with 
| V n -> let rec checkenv e =
  match e with
  | [] -> false
  | hd::tl -> 
    if n = hd then true else checkenv tl in checkenv e
| P(v,l) -> checklam (v::e) l
| C(l1,l2) -> 
(checklam e l1) && (checklam e l2) in checklam [] lam 
