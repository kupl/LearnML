type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec checkMetro met =
    let rec aux m ids =
        match m with
        | STATION id -> List.mem id ids
        | AREA (id, m') -> aux m' (id::ids)
        | CONNECT (m1, m2) -> (aux m1 ids) && (aux m2 ids) 
		in aux met []
