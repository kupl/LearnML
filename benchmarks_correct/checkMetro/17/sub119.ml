type metro = STATION of name
                    | AREA of name * metro
                    | CONNECT of metro * metro
and name = string

let rec stationinList : metro * string list -> bool = fun c ->
    match c with
   |(STATION a, b) -> List.mem a b
   |(AREA(a,b), c) -> stationinList(b, a::c)
   |(CONNECT(a,b),c) -> stationinList(a,c) && stationinList(b,c)
    
let rec checkMetro : metro -> bool = fun c ->
    match c with
    |a -> stationinList(a, [])