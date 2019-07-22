type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec checkMetro(metro) = 
    match metro with
    | AREA(a, STATION b) -> a = b
    | AREA(a, CONNECT(b, c))
      -> checkMetro(AREA(a, b)) && checkMetro(AREA(a, c))
    | AREA(a, AREA(b, c)) -> checkMetro(AREA(a, c))
    | CONNECT(_, _) -> false
    | STATION(_) -> false
