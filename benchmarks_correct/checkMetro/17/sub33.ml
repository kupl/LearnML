type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
       and name = string

let checkMetro (m : metro) : bool = 
    let rec checkArea ((m : metro), (names : name list)) : bool =
        match m with
        | STATION n -> List.mem n names
        | AREA (n, m1) -> checkArea (m1, names @ [n]) 
        | CONNECT (m1, m2) -> (checkArea (m1, names)) && (checkArea (m2, names))
    in
    checkArea (m, [])

