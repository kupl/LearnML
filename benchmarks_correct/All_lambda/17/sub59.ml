type var = string

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda

let rec check2 (m : lambda) (l : string list) : bool =
  match m with
  | V a -> if List.mem a l then true else false
  | P (a, b) -> check2 b (l @ [a])
  | C (a, b) -> (check2 a l) && (check2 b l)

let check (m : lambda) : bool =
  check2 m []
