type metro = STATION of name
|AREA of name * metro
|CONNECT of metro * metro
and name = string

let rec checkMetro : metro -> bool = fun met ->
match met with
|STATION n -> false
|AREA (name , met) -> checkArea(met,name::[])
|CONNECT (m1,m2) -> checkMetro m1 && checkMetro m2
and checkArea : metro * string list -> bool = fun (m,nl) ->
match (m,nl) with
|(STATION name,[]) -> false 
|(STATION name,n::nl) -> if n=name then true else checkArea(STATION name,nl)
|(CONNECT(m1,m2),nl) -> checkArea(m1,nl) && checkArea(m2,nl)
|(AREA(name,met),nl) -> checkArea(met,name::nl)
