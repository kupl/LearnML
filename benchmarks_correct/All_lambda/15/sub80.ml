(*
  CSE/2015-21233/김종권
  Homework 2-3
*)
type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec check' m l =
  match m with
  | V var ->
    List.exists (fun x -> x = var) l
  | P (var, m') ->
    check' m' (var :: l)
  | C (m1, m2) ->
    (check' m1 l) && (check' m2 l)
      
let check m =
  check' m []

