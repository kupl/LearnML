type metro = STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let streq a b =
  a = b

let rec checkMetro_lst mtr lst = 
  match mtr with
    | STATION x -> List.exists (streq x) lst
    | AREA (x, y) -> checkMetro_lst y (x::lst)
    | CONNECT (x, y) -> (checkMetro_lst x lst) && (checkMetro_lst y lst)

let checkMetro mtr = 
  checkMetro_lst mtr []

