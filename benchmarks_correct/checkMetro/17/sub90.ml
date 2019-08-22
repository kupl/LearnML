type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string

let rec checkList ((m : lambda), (l : var list)) : bool =
    match m with
    | V (n1) -> if (List.mem n1 l) then true
                      else false
    | P (n1, m1) -> checkList (m1, n1::l)
    | C (m1, m2) -> checkList (m1, l) && checkList (m2, l    )

let check (m : lambda) : bool =
    checkList (m, [])