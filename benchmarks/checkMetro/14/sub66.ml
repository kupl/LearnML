type metro = STATION of name
	|AREA of name * metro
	|CONNECT of metro * metro
and name = string

let rec contains listOfArea s =
	match listOfArea with
	|[] -> false
	|h::t -> (if h = s then true
		else contains t s)

let rec checkMetro2 x listOfArea =
	match x with
	|STATION s -> contains listOfArea s
	|AREA (n, m) -> checkMetro2 m (n::listOfArea)
	|CONNECT (m1, m2) -> checkMetro2 m1 listOfArea && checkMetro2 m2 listOfArea

let checkMetro x =
	match x with
	|STATION s-> true
	|_ -> checkMetro2 x []