(*컴퓨터공학부/2011-11729/안진우/2-4*)


type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec check_h ((x: lambda), (l: var list)) : bool =
        match x with
        | V n -> if List.mem n l then true else false
        | P (n, m) -> check_h (m, List.append [n] l)
        | C (m1, m2) -> (check_h (m1, l)) && (check_h (m2, l))

let check (x: lambda) : bool =
        check_h (x, [])
