type lambda =
    V of var
  | P of var * lambda
  | C of lambda * lambda

and var = string


let check m =
  let rec check2 m set =
    match m with
    | V n -> List.mem n set
    | P (n, m) -> check2 m (n::set)
    | C (m1, m2) -> check2 m1 set && check2 m2 set
  in
  check2 m []
