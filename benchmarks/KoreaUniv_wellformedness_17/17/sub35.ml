(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda

let rec scan x l =
  match l with
  |[] -> false
  |hd::tl -> if x=hd then true else scan x tl

let rec well_formed exp l =
match exp with
  |V x -> if scan x l then true else false
  |P (x, lam) -> well_formed lam (l@[x])
  |C (lam1, lam2) -> (well_formed lam1 l)&&(well_formed lam2 l)

let rec check : lambda -> bool
= fun lam -> well_formed lam []









