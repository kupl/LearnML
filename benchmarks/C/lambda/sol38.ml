type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string


let check m_ =
	let p n1 n2= n1=n2 in
	let rec mChecker m alst = 
		match m with
		V n -> 
			List.exists (p n) alst
		| P (n, met) -> mChecker met ([n]@alst)
		| C (met1, met2) -> (mChecker met1 alst) & (mChecker met2 alst)
	in
	mChecker m_ []

