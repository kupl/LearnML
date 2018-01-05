type metro =
  STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec checkstation : string * metro -> bool =
(
	function (x, y) ->
		match y with
		STATION a -> if String.compare x a == 0 then true else false
		| AREA (a, b) -> checkstation(x, b)
		| CONNECT (a, b) -> (checkstation (x, a) || checkstation (x, b))
);;

let rec checkmetro : metro -> bool =
(
	function a ->
		match a with
		STATION x -> true
		| AREA (x, y) -> checkstation (x, y) && checkmetro (y)
		| CONNECT (x, y) -> checkmetro (x) && checkmetro (y)
);;