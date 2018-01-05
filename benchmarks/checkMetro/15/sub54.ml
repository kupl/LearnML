type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec findStation: metro -> string list = fun x ->
  match x with
  | STATION station -> [station]
  | CONNECT(a, b) -> (findStation a)@(findStation b)
  | AREA(area, next) -> findStation next

let rec exclude: metro -> string list -> bool = fun m strl ->
  match strl with
  | [] -> true
  | _ ->
    (match m with
     | STATION sta -> false
     | AREA(area, next) ->
	if (List.mem area strl) then (exclude next (List.filter ((<>) area) strl))
	else (exclude next strl)
     | CONNECT(STATION sta, con2) -> exclude con2 strl
     | CONNECT(con1, STATION sta) -> exclude con1 strl
     | CONNECT(con1, con2) -> (exclude con1 (findStation con1)) && (exclude con2 (findStation con2))
    )

let rec checkMetro: metro -> bool = fun x ->
  exclude x (findStation x)
