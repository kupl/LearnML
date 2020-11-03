type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string;;

let check lambda =
  let rec check2 met ids =
    match met with
    | V id -> List.mem id ids
    | P (id, m) -> check2 m (id :: ids)
    | C (m, m') -> check2 m ids && check2 m' ids
  in
  check2 lambda [];;
