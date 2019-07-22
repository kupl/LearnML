type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro and name = string

let rec checkMetro2 : (metro * string list) -> bool = fun (m,s) -> match m with
  |STATION k -> if List.mem k s then true else false
  |AREA (k,q) -> checkMetro2 (q, k::s)
  |CONNECT (k,q) -> checkMetro2 (k,s) && checkMetro2 (q,s)

let rec checkMetro m = checkMetro2 (m, [])


