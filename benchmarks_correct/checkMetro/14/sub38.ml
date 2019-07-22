type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

(* Make a list and match *)
let checkMetro (m: metro): bool =
  let rec checkMetro_list (m: metro) (l: name list): bool =
    match m with
    | STATION name1 -> List.mem name1 l
    | AREA (name1, metro1) -> checkMetro_list metro1 (name1::l)
    | CONNECT (metro1, metro2) -> checkMetro_list metro1 l && checkMetro_list metro2 l
  in
  checkMetro_list m []
