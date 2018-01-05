type name = string

type metro = STATION of name
		   | AREA of name * metro
		   | CONNECT of metro * metro

let rec checkMetro_list x l =
  match x with
  | STATION n -> List.exists (fun m -> (n = m)) l
  | AREA (n, m) -> checkMetro_list m (List.append l [n])
  | CONNECT (m1, m2) -> (checkMetro_list m1 l) && (checkMetro_list m2 l)

let checkMetro x = checkMetro_list x [] 
