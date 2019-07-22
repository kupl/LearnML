type metro = STATION of name
|AREA of name * metro
|CONNECT of metro * metro
and name = string

let checkMetro metro =
let rec real_cM metro lst =
match metro, lst with
|STATION s, l -> (List.mem s l)
|AREA (name, met), l -> (real_cM met (l@[name]))
|CONNECT (met1, met2), l -> (real_cM met1 l) && (real_cM met2 l)
in real_cM metro []
