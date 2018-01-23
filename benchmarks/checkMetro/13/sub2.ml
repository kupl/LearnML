type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let checkMetro (m : metro) : bool =
    (
        let rec checkMetroRec (m : metro) (s : name list) : bool =
            (
                match m with
                    STATION (n) -> (List.mem n s)
                |   AREA ((n), (m)) -> (checkMetroRec m s)
                |   CONNECT ((m1), (m2)) ->
                        (checkMetroRec m1 s) && (checkMetroRec m2 s)
            ) in
        (checkMetroRec m [])
    )
