type metro = STATION of name
        | AREA of name * metro
        | CONNECT of metro * metro
    and name = string

let checkMetro met =
    let rec checkInclude m l =
        match m with
        | STATION s -> List.mem s l
        | AREA (a, inner_m) -> checkInclude inner_m (a::l)
        | CONNECT (a, b) -> (checkInclude a l) && (checkInclude b l)
    in checkInclude met [];;