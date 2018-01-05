type metro  = STATION of name
            | AREA of name * metro
            | CONNECT of metro * metro
and name = string

let rec checkhelper(x, l) =
    match x with
    | STATION y -> (List.mem y l)
    | AREA (y, z) -> checkhelper(z, (y :: l))
    | CONNECT (y, z) -> (checkhelper(y, l) && checkhelper(z, l))

let checkMetro x =
    checkhelper(x, [])
