type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec check mt arealist =
    match mt with
    | STATION str ->
            List.exists (fun x -> x=str) arealist
    | CONNECT (m1,m2) ->
            (check m1 arealist) && (check m2 arealist)
    | AREA  (str, m) ->
            check m (str::arealist)

let checkMetro met =
    check met []
