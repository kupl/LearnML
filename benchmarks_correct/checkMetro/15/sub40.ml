type metro = STATION of name
 | AREA of name * metro
 | CONNECT of metro * metro
and name = string

let rec check : (string list)*metro -> bool = fun (nlist,m) ->
 if nlist=[] then true
 else match m with
 | STATION(n) -> (List.exists (fun x -> n=x) nlist)
 | AREA(n,m1) -> check(n::nlist,m1)
 | CONNECT(m1,m2) -> check(nlist,m1) && check(nlist,m2)

let rec checkMetro : metro -> bool = fun m ->
 match m with
 | STATION(n) -> false
 | AREA(n,m1) -> check(n::[],m1)
 | CONNECT(m1,m2) -> checkMetro(m1) && checkMetro(m2)