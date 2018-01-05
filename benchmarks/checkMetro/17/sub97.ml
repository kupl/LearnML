type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let same y = fun x -> ((String.compare x y) == 0)

let rec checkMetroName (x, y) = match y with
							|STATION a -> List.exists (same a) x
							|AREA (a, b) -> checkMetroName(List.append x [a], b)
							|CONNECT (a, b) -> checkMetroName(x, a) && checkMetroName(x, b)

let rec checkMetro x = match x with
						|STATION a -> false
						|CONNECT (a, b) -> (checkMetro a) && (checkMetro b)
						|AREA (a, b) -> checkMetroName ([a], b)

