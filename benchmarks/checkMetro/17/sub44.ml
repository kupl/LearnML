type metro = STATION of name
            | AREA of name * metro
            | CONNECT of metro * metro
and name = string

let rec checkMetroImpl (m : metro) (l : 'a list) : bool = 
    match m with
    | STATION n -> List.mem n l
    | AREA (n, mm) -> checkMetroImpl mm (n::l)
    | CONNECT (m1, m2) -> (checkMetroImpl m1 l) && (checkMetroImpl m2 l)

let checkMetro (m : metro) : bool = checkMetroImpl m []
