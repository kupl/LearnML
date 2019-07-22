type metro = STATION of name 
	| AREA of name * metro
    | CONNECT of metro * metro
and name = string

let checkMetro metro =
	let rec check met str_list=
	match met with
	| STATION a -> List.mem a str_list
	| AREA (a,STATION b) -> (a=b)||(List.mem b str_list)
	| AREA (a,b) -> check b (str_list@[a])
	| CONNECT (a,b) -> (check a str_list)&&(check b str_list)
	in
	check metro []

