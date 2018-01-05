type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let checker (a:name) (b:name) : bool =
    if a = b then true
    else false

let rec checkMetroHelper ((a:metro),(l:name list)) : bool =
    match a with
    | STATION x -> List.exists(checker x) l
    | AREA(x,y) -> checkMetroHelper(y,x::l)
    | CONNECT(x,y) -> checkMetroHelper(x,l) && checkMetroHelper(y,l)

let checkMetro (a:metro) : bool =
    checkMetroHelper(a,[])
