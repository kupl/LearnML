type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let checkMetro (m : metro) : bool =
    (
        let rec checkMetroRec (m : metro) (s : name list) : bool =
            (
                match m with
                    STATION (n : name) -> (List.mem n s)
                |   AREA ((n : name), (m : metro)) -> (checkMetroRec m s)
                |   CONNECT ((m1 : metro), (m2 : metro)) ->
                        (checkMetroRec m1 s) && (checkMetroRec m2 s)
            ) in
        (checkMetroRec m [])
    )
