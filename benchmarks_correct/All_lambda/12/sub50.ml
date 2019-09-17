type lambda =
    V of var
  | P of var * lambda
  | C of lambda * lambda

and var = string


let check m =
  let rec check m set =
    match m with
    | V n -> List.mem n set
    | P (n, m) -> check m (n::set)
    | C (m1, m2) -> check m1 set && check m2 set
  in
  check m []
