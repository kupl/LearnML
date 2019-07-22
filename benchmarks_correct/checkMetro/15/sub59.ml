type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string;;

let rec checkMetro2 m l =
match m with
| STATION n1 -> List.exists (fun x -> x = n1) l 
| AREA (n1, m1) -> checkMetro2 m1 (n1::l)
| CONNECT (m1, m2) -> checkMetro2 m1 l && checkMetro2 m2 l

let checkMetro metro = checkMetro2 metro []


