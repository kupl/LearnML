(**********************)
(*   Problem 2        *)
(**********************)


type lambda =  V of var
              | P of var * lambda
              | C of lambda * lambda
and var = string
type bound = var list
let empty_bound = []
let rec apply_bound : var->bound->bool 
= fun v bnd ->
  match bnd with
    |[]->false
    |hd::tl -> if hd=v then true
                else apply_bound v tl

let extend_bound : var->bound->bound 
= fun v bnd ->
  v::bnd
let rec sub_check : lambda->bound->bool 
= fun lam bnd ->
  match lam with
    |V v -> apply_bound v bnd
    |P (v,lamb) -> let bnd_n = extend_bound v bnd in
                      sub_check lamb bnd_n
    |C (lamb1,lamb2) -> if sub_check lamb1 bnd = true && sub_check lamb2 bnd = true then true
                            else false

let rec check : lambda -> bool
= fun lam -> sub_check lam []