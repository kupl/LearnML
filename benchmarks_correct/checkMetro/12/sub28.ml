type metro = STATION of name | AREA of name * metro | CONNECT of metro * metro
and name = string
let checkMetro met = 
	let rec cm nowl mect =
		match mect with
		|STATION a -> (List.mem a nowl)
		|AREA(n,m) -> (cm (n::nowl) m)
		|CONNECT(a,b)-> (cm nowl a) && (cm nowl b)
	in
	cm [] met


