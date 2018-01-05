type metro =
STATION of string
|AREA of string*metro
|CONNECT of metro*metro;;

let checkMetro metro =
	let rec chk what at =
		match at with
		|hd::tl -> if hd = what then true else (chk what tl)
		|[] -> false
	in
	let rec foo mtr lst = 
		match mtr with
		|STATION n -> (chk n lst)
		|AREA (n,rest) -> (foo rest (n::lst))
		|CONNECT (a,b) -> (foo a lst)&&(foo b lst)
	in
	(foo metro [])
;;

