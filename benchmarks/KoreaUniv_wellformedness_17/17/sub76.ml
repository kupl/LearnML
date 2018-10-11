(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam ->
  let rec check2 : lambda -> var list -> bool -> bool
  = fun lamb lst b ->
  match lamb with 
  | V x -> 
  (let rec helpfunc2 : var-> var list -> bool
  = fun v vlst ->
    (match vlst with
    |[] -> false
    |hd::tl -> if (v=hd) then true else helpfunc2 v tl) in helpfunc2 x lst)
  | P (v, l) -> (check2 l (v::lst) b) && b 
  | C (l1, l2) -> ((check2 l1 lst b) && (check2 l2 lst b)) && b in check2 lam [] true