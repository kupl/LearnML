(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type lenv = var list
let empty_lenv = []
let rec apply_lenv e x =
  match e with
  |[] -> false
  |y::tl -> if x = y then true else apply_lenv tl x

let rec pre_check : lambda -> lenv -> bool
= fun ld le ->
  match ld with
  |V x -> apply_lenv le x
  |P (v,l) ->
    let le' = v::le in
      pre_check l le'
  |C (l1,l2) ->
    (pre_check l1 le) && (pre_check l2 le)

let rec check : lambda -> bool
= fun ld ->
  pre_check ld empty_lenv

