(*2009-11718 박준상 2-1*)

type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string


let rec checkArea (var, lst) =
	match lst with
	| [] -> false
	| hd::tl -> if hd=var then true
				else checkArea (var, tl)


let rec check lambda =
	let rec checkStation lst met =
		match met with
		| V n -> checkArea (n, lst)
		| P (n, m) ->  checkStation (n::lst) m
		| C (m1, m2) -> (checkStation lst m1)&&(checkStation lst m2) in
(*lst 에 커넥트 양쪽거 다들어가있지않나*)
	checkStation [] lambda
