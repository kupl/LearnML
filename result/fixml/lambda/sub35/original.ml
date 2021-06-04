type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec dec met lst =
  match met with
  | V a -> if List.mem a lst = true then true else false
  | C (m1, m2) -> dec m1 lst && dec m1 lst
  | P (n1, m1) -> dec m1 (n1 :: lst)


let rec check m =
  match m with
  | V a -> false
  | P (n1, m1) -> dec m [ n1 ]
  | C (m1, m2) -> dec m1 [] && dec m2 []
