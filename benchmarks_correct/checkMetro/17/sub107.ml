 type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro
 and name = string

let rec checkMetroWList ((ilist : string list),(met : metro)) : bool = 
match (ilist,met) with
|(lst,AREA(id,m)) -> checkMetroWList(id::lst, m)
|(lst,STATION id) -> List.mem id lst
|(lst,CONNECT(m1,m2)) -> checkMetroWList(lst,m1) && checkMetroWList(lst,m2)

let checkMetro (met : metro) : bool = 
checkMetroWList([],met)

