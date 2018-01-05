type metro = STATION of name
	|AREA of name * metro
	|CONNECT of metro * metro
  and name = string

let rec check lst name metro =
	
	let mklst lst x = x::lst
	in
	match metro with	
	|STATION n -> if (List.mem n lst) then true else false
	|AREA(n,m) -> (check (mklst lst n) name m) || (check (mklst lst n) n m)
	|CONNECT(m1,m2) -> (check lst name m1) && (check lst name m2)

let checkMetro metro = match metro with
	|STATION n -> false
	|AREA(n,m) -> (check [n] n m)
	|CONNECT(m1,m2) -> false
