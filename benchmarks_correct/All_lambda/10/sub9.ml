type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string;;

let check lambda =
  let rec check met ids =
    match met with
    | V id -> List.mem id ids
    | P (id, m) -> check m (id :: ids)
    | C (m, m') -> check m ids && check m' ids
  in
  check lambda [];;
