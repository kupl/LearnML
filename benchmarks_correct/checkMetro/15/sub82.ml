type metro =
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let checkMetro metro =
  let rec checkMetro_helper m name_list = 
    match m with
    |STATION name -> List.mem name name_list
    |AREA (name, m1) -> checkMetro_helper m1 (name::name_list)
    |CONNECT (m1, m2) -> (checkMetro_helper m1 name_list) && (checkMetro_helper m2 name_list)
  in
  checkMetro_helper metro []

