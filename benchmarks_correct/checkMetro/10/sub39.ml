type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string


let checkMetro m_ =
	let p n1 n2= n1=n2 in
	let rec mChecker m alst = 
		match m with
		STATION n -> 
			List.exists (p n) alst
		| AREA (n, met) -> mChecker met ([n]@alst)
		| CONNECT (met1, met2) -> (mChecker met1 alst) & (mChecker met2 alst)
	in
	mChecker m_ []

