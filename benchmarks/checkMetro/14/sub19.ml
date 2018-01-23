type metro = STATION of name
	     | AREA of name * metro
	     | CONNECT of metro * metro
and name = string

let compareId (idStation: string) (idArea: string): bool = 
  if (idStation = idArea) then true
  else false

let rec checkArea (idList: string list) (m: metro): bool = 
  match m with
  | STATION id -> List.exists (compareId id) idList
  | AREA (id, m1) -> (checkArea (id::idList) m1)
  | CONNECT (m1, m2) -> (checkArea idList m1) && (checkArea idList m2)

let rec checkMetro (m: metro): bool = 
  match m with
  | STATION id -> false
  | AREA (id, m1) -> (checkArea [id] m1) 
  | CONNECT (m1, m2) -> (checkArea [] m1) && (checkArea [] m2)
