(**********************)
(*   Problem 2        *)
(**********************)
type lambda = V of var (* Var *)
            | P of var * lambda (* Proc/ Application *)
            | C of lambda * lambda (* Call *)
and var = string
and bindings = var list

(* bindings *)
let empty_bindings = []
let make_binding v b = v::b
let rec bound b bindings = 
  match bindings with
  | [] -> false
  | v::tl -> if v = b then true else bound b tl

let rec checkBindings : lambda -> bindings -> bool
= fun lam bindings ->
match lam with
 | V v -> bound v bindings
 | P (v,lam) -> checkBindings lam (make_binding v bindings)
 | C (lam1, lam2) -> checkBindings lam1 bindings && checkBindings lam2 bindings

let rec check : lambda -> bool
= fun lam -> checkBindings lam empty_bindings
