type metro =
STATION of string
|AREA of string*metro
|CONNECT of metro*metro;;

let checkMetro metro =
	let rec chk st at =
		match at with
		|hd::tl -> if hd = st then true else (chk st tl)
		|[] -> false
	in
	let rec foo mtr lst = 
		match mtr with
		|STATION s -> (chk s lst)
		|AREA (a,rest) -> (foo rest (a::lst))
		|CONNECT (a,b) -> (foo a lst)&&(foo b lst)
	in
	(foo metro [])
;;

