type metro =
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let checkMetro (m : metro) : bool =
  let rec loop area metro =
    match metro with
    | STATION name -> List.exists (fun x -> x = name) area
    | AREA (name, metro') -> loop (name :: area) metro'
    | CONNECT (metro1, metro2) -> (loop area metro1) && (loop area metro2) in
  loop [] m

