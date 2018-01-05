type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let rec list_del lst element =
	match lst with
	| [] -> []
	| head :: tail -> if head = element then list_del tail element
			  else head :: (list_del tail element)

let rec remainStation m =
	match m with
	| STATION str -> [str]
	| AREA (str, met) -> list_del (remainStation met) str
	| CONNECT (met1, met2) -> (remainStation met1) @ (remainStation met2)

let rec checkMetro m =
	if (List.length (remainStation m)) = 0 then true
	else false

