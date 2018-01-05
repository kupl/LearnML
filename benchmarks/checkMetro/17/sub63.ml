type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec checkmetrolist ((m: metro), (l: string list)) : bool =
     match m with
     | STATION x -> List.mem x l
     | AREA (a, m') -> checkmetrolist(m', l@[a])
     | CONNECT (x, y) -> (checkmetrolist(x, l) && checkmetrolist(y, l))

let checkMetro (m: metro) : bool =
     checkmetrolist(m, [])
