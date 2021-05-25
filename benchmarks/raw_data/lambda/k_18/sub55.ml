type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec checklambda : var list -> lambda -> bool
=fun lst lam -> match lam with
  | V x -> List.mem x lst
  | P (x, l) -> checklambda (x::lst) l
  | C (l1, l2) -> checklambda lst l1 && checklambda lst l2

let check : lambda -> bool
= fun lam -> checklambda [] lam;;