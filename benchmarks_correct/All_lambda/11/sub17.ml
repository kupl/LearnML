
type lambda = V of var
	   | P of var * lambda
	   | C of lambda * lambda
and var = string

let check m =

	let rec test(a, lst) =
        	match lst with
                	| x::l -> (if (a=x) then true
                           	   else test(a, l))
                	| [] -> false
	in

	let rec check2(m, lst)=
	        match m with
        	        | V a -> test(a, lst)
	               	| P(a, met) -> check2(met, a::lst)
	                | C(m1, m2) -> (check2(m1,lst) & check2(m2,lst))
	in

	check2(m, [])
