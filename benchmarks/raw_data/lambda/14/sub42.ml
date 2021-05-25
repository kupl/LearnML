type var = string

type lambda = V of var
           | P of var * lambda
	   | C of lambda * lambda
(*
 * manually edited
 * checkMetro -> check
 * check -> check_sub
 *)
let rec check_sub m l =
  match m with
  | V id -> (List.mem id l)
  | C (m1, m2) -> ((check_sub m1 l) && (check_sub m2 l))
  | P (id, met) -> (check_sub met (id::l))

let rec check m =
  match m with
  | V id -> false
  | C (m1, m2) -> ((check m1) && (check m2))
  | P (id, met) -> (check_sub met [id])
