type name = string
type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro

let rec isStationIn (areas, n) =
  match areas with
  | [] -> false
  | hd::tl -> (hd = n) || (isStationIn (tl, n))

let rec checkMetro_real (areas, metro) =
  match metro with
  | AREA (a, m) -> checkMetro_real (a::areas, m)
  | STATION n -> isStationIn (areas, n)
  | CONNECT (a, b) -> checkMetro_real (areas, a) && checkMetro_real (areas, b)

let checkMetro metro = checkMetro_real ([], metro)
