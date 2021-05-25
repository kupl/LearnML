(* 2009-11674 ����� HW1-1*)

let sigma(a, b, f) =

	let rec sigma2(a, b, f, sum) =
        	if (a = b) then (sum + f(a))
        	else sigma2((a+1), b, f, (sum + f(a)))
	in

	if (a > b) then raise(Invalid_argument "sigma")
	else sigma2(a, b, f, 0)
