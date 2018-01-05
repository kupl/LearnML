type metro = 
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let checkMetro (x: metro): bool = 
  let rec checkMetroRec (x: metro) (y: name list): bool = match x with 
  | STATION a -> List.exists (fun x -> x = a) y
  | AREA (a, b) -> checkMetroRec b (a :: y)
  | CONNECT(a, b) -> (checkMetroRec a y) && (checkMetroRec b y)
  in checkMetroRec x []
