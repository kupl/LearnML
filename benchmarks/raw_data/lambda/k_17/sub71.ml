(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam ->
  let rec checker : lambda -> var list -> bool -> bool
  = fun lam1 lst b ->
  match lam1 with 
  | V x -> 
  (let rec help : var-> var list -> bool
  = fun v vlst ->
    (match vlst with
    |[] -> false
    |hd::tl -> if (v=hd) then true else help v tl) in help x lst)
  | P (v, l) -> (checker l (v::lst) b) && b 
  | C (l1, l2) -> ((checker l1 lst b) && (checker l2 lst b)) && b in checker lam [] true

