type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec checkMetroHelp ((x: metro), (l: 'a list)): bool =
  match x with
  | STATION s -> List.mem s l
  | CONNECT (metro1, metro2) -> (checkMetroHelp (metro1, l)) && (checkMetroHelp (metro2, l))
  | AREA (s, metro) -> checkMetroHelp (metro, s::l)

let rec checkMetro (x: metro): bool = 
  checkMetroHelp (x, [])
