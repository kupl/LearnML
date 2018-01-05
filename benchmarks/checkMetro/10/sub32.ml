
type name = string
type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro

let checkmetro metro =
	let rec check metro table =
		match metro with
		STATION(s)->(List.mem s table)
		|AREA(n,m)->check m (n::table)
		|CONNECT(x,y)->((check x table)&&(check y table))
		in
	check metro []
	
