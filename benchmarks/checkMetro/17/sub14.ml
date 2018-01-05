(*컴퓨터공학부/2011-11729/안진우/2-4*)


type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec checkMetro_h ((x: metro), (l: name list)) : bool =
        match x with
        | STATION n -> if List.mem n l then true else false
        | AREA (n, m) -> checkMetro_h (m, List.append [n] l)
        | CONNECT (m1, m2) -> (checkMetro_h (m1, l)) && (checkMetro_h (m2, l))

let checkMetro (x: metro) : bool =
        checkMetro_h (x, [])
