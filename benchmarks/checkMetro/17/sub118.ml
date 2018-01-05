type metro =
    | STATION of name
    | AREA of name * metro
    | CONNECT of metro * metro
and name = string
and areaList = string list;;

let rec isIncluded (n: name) (al: areaList) : bool =
    match al with
    | [] -> false
    | hal :: tal -> if hal = n then true else isIncluded n tal;;

let rec scan (m: metro) (al: areaList) : bool =
    match m with
    | STATION(n) -> isIncluded n al
    | AREA(n, mm) -> scan mm (al @ [n])
    | CONNECT(m1, m2) -> (scan m1 al) && (scan m2 al);;

let rec checkMetro (m: metro) : bool =
    scan m [];;
