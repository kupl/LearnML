type metro = STATION of name
        | AREA of name * metro
        | CONNECT of metro * metro
and name = string

let compareList (name, l) =
        List.mem name l

let rec checkMetroTail (metro, l) = match metro with
        STATION (x) -> compareList(x, l)
        | AREA (x, y) -> checkMetroTail(y, x::l)
        | CONNECT (x, y) -> checkMetroTail(x, l) && checkMetroTail(y, l)

let checkMetro metro = let l = [] in match metro with
         STATION (x) -> compareList(x, l)
        | AREA (x, y) -> checkMetroTail(y, x::l)
        | CONNECT (x, y) -> checkMetroTail(x, l) && checkMetroTail(y, l)
