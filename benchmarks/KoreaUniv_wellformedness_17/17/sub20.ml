(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> let rec rm v lst = match lst with
|hd::tl -> if hd = v then rm v tl else hd:: rm v tl
|[] -> [] in
let rec mklst lam lst = match lam with
|V(v) -> v::lst
|P(v,l) -> rm v (mklst l lst)
|C(l1,l2) -> (mklst l1 lst) @ (mklst l2 lst) in if mklst lam [] = [] then true else false
