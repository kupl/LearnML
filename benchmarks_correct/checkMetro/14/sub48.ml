type metro = STATION of name
            | AREA of name * metro
            | CONNECT of metro * metro
and name = string

let rec checkInner: metro -> string list -> bool =
    fun m sl ->
        match m with
        | STATION n -> (List.mem n sl)
        | AREA (n, m) -> (checkInner m (n::sl))
        | CONNECT (m1, m2) -> (checkInner m1 sl)&&(checkInner m2 sl)

let checkMetro: metro -> bool =
    fun m ->
        (checkInner m [])
