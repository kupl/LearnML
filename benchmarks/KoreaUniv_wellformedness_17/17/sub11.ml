
(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> let rec find v lst = match lst with
                                  | hd::tl -> if hd = v then true else find v tl
                                  | [] -> false
          in let rec func lam lst = match lam with
            | V v -> if find v lst then true else false
            | P (v,l) -> func l (v::lst)
            | C (l1,l2) -> func l1 lst  && func l2 lst
          in func lam []
