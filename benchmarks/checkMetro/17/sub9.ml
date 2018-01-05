type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro
and name = string

let rec checkMetro (m : metro) =
let rec evalMetro ((areas: name list), (m_eval : metro)) = match m_eval with
| STATION(sname) -> List.mem sname areas
| AREA(aname, m_prime) -> evalMetro((aname :: areas), m_prime)
| CONNECT(m1, m2) -> evalMetro(areas, m1) && evalMetro(areas, m2)
in evalMetro([], m)
