type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (m : lambda) : bool =
  match m with
  | V n -> true
  | C (m1, m2) -> check m1 && check m2
  | P (n, m) -> (
      match m with V n2 -> if n = n2 then true else false | _ -> check m )
