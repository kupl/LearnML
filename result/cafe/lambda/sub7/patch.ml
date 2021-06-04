type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (met : lambda) : bool =
  let rec check2 (met : lambda) (lst : string list) : bool =
    match met with
    | V id -> List.mem id lst
    | P (id, m) -> check2 m (lst @ [ id ])
    | C (m1, m2) -> check2 m1 lst && check2 m2 lst
  in

  match met with
  | V id -> false
  | P (id, m) -> check2 m [ id ]
  | C (m1, m2) -> check m1 && check m2