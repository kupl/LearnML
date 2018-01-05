type metro = STATION of name
        | AREA of name * metro
        | CONNECT of metro * metro
and name = string

let rec checkMetro m =
        let rec checkArea (m, nl) =
                match m with
                | STATION n -> (List.mem n nl)
                | AREA (n, m) -> (checkArea (m, (n::nl)))
                | CONNECT (m1, m2) -> (checkArea (m1, nl)) && (checkArea (m2, nl))
        in

        (checkArea (m, []))
