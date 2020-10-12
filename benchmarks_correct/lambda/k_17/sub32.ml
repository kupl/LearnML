(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec find x arr=
  match arr with
  []->false
  |hd::tl-> if hd=x then true else find x tl


let rec apply lam var =
  match lam with
  V x-> find x var
  |P(x,l)-> apply l (x::var)
  |C(l1,l2)-> apply l1 var&&apply l2 var


let rec check : lambda -> bool
= fun lam ->
 match lam with
 V x-> false
 |P(x,l)-> apply l (x::[])
 |C(l1,l2)-> check l1&& check l2