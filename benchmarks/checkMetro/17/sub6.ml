type metro = STATION of name
            | AREA of name * metro
            | CONNECT of metro * metro
and name = string

let rec isInclude: name * metro -> bool = fun (n, m) ->
    match m with
    | STATION a -> (n == a)
    | AREA (a, b) -> (isInclude (a, b)) && (isInclude (n, b))
    | CONNECT (a, b) -> (isInclude (n, a)) || (isInclude (n, b))

let checkMetro: metro -> bool = fun x ->
    match x with
    | AREA (a, STATION b) -> (a == b)
    | AREA (a, b) -> isInclude(a, b)
    | _ -> false
