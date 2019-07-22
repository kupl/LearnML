type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let checkMetro m =
    let rec check(m, l) = match m with
    | STATION(n) -> List.mem n l
    | AREA(n, m1) -> check(m1, n::l)
    | CONNECT(m1, m2) -> check(m1, l) && check(m2, l)
    in
    check(m, [])
