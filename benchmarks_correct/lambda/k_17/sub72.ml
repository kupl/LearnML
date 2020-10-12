(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam ->
  let rec find x l =
    match l with
    | [] -> false
    | h::t -> if x = h then true else find x t in
  let rec check2 la l =
    match la with
    | V v -> find v l
    | P (v, la1) -> check2 la1 (v::l)
    | C (la1, la2) -> (check2 la1 l) && (check2 la2 l) in
  check2 lam []