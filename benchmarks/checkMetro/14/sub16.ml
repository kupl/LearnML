(*2009-11718 박준상 2-1*)

type metro = STATION of name
			| AREA of name * metro
			| CONNECT of metro * metro
and name = string


let rec checkArea (name, lst) =
	match lst with
	| [] -> false
	| hd::tl -> if hd=name then true
				else checkArea (name, tl)


let rec checkMetro metro =
	let rec checkStation lst met =
		match met with
		| STATION n -> checkArea (n, lst)
		| AREA (n, m) ->  checkStation (n::lst) m
		| CONNECT (m1, m2) -> (checkStation lst m1)&&(checkStation lst m2) in
(*lst 에 커넥트 양쪽거 다들어가있지않나*)
	checkStation [] metro
