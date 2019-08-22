type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec check_aux: lambda -> var list -> bool =
  fun m nl ->
    match m with
    | V n -> List.exists (fun n1 -> (n=n1)) nl
    | P (n, m1) -> check_aux m1 (n::nl)
    | C (m1, m2) -> (check_aux m1 nl) && (check_aux m2 nl)

let check: lambda -> bool =
  fun m ->
    check_aux m []
