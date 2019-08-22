type var = string

type lambda = V of var
           | P of var * lambda
	   | C of lambda * lambda

let rec check m l =
  match m with
  | V id -> (List.mem id l)
  | C (m1, m2) -> ((check m1 l) && (check m2 l))
  | P (id, met) -> (check met (id::l))

let rec check m =
  match m with
  | V id -> false
  | C (m1, m2) -> ((check m1) && (check m2))
  | P (id, met) -> (check met [id])
