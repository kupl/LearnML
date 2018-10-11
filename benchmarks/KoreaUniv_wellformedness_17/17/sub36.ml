(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string
type bound = var list
let empty_bound = []
let rec apply_bound : var->var list->bool = fun x bnd ->
  match bnd with
    |[]->false
    |a::tl -> if a=x then true
                else apply_bound x tl
;;
let update_bound : var->var list->var list = fun x bnd ->
  x::bnd;;  
let rec sub_check : lambda->bound->bool = fun lam bnd ->
  match lam with
    |V x -> apply_bound x bnd
    |P (x,lamb) -> let bnd1 = update_bound x bnd in
                      sub_check lamb bnd1
    |C (lamb1,lamb2) -> if sub_check lamb1 bnd = true && sub_check lamb2 bnd = true then true
                            else false
 ;;
let rec check : lambda -> bool
= fun lam -> sub_check lam [];;
 (* TODO *)

