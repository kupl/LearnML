type name = string

type metro = STATION of name
            | AREA of name * metro
            | CONNECT of metro * metro

let rec checkMetro2 (m : metro) (l : string list) : bool =
  match m with
  | STATION a -> if List.mem a l then true else false
  | AREA (a, b) -> checkMetro2 b (l @ [a])
  | CONNECT (a, b) -> (checkMetro2 a l) && (checkMetro2 b l)

let checkMetro (m : metro) : bool =
  checkMetro2 m []
