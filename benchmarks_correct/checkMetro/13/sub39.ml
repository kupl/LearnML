type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
    and name = string

let checkMetro m =
    let rec checkM (m, l) =
        match m with
        | STATION n -> List.mem n l
        | AREA (name, metro) -> checkM (metro, name::l)
        | CONNECT (metro1, metro2) -> checkM (metro1, l) && checkM (metro2, l)
    in
    checkM (m, [])
