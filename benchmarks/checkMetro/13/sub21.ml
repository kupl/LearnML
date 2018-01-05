type metro = STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let rec isthere s lst =
	match lst with
	|[] -> false
	|hd::tl -> if (hd = s) then true
		   else (isthere s tl)

let rec check mtr lst =
	match mtr with
	|STATION x -> (isthere x lst)
	|AREA (str,m) -> (check m (str::lst))
	|CONNECT (m1,m2) -> (check m1 lst) && (check m2 lst)

let checkMetro m =
	check m []


