type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec checkMetroByList (m, l) =
    match m with
    | STATION a -> List.exists (fun x -> x = a) l
    | AREA (n, a) -> checkMetroByList (a, l @ [n])
    | CONNECT (a, b) -> checkMetroByList (a, l) && checkMetroByList (b, l)

let checkMetro m = checkMetroByList (m, [])
