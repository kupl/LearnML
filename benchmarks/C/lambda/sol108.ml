type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let check m =
    let rec check2(m, l) = match m with
    | V(n) -> List.mem n l
    | P(n, m1) -> check2(m1, n::l)
    | C(m1, m2) -> check2(m1, l) && check2(m2, l)
    in
    check2(m, [])
