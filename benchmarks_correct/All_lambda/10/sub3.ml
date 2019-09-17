(* CSE/ 2004-11920 / Yeseong Kim/ Prob 7*)

type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let check m =
		let rec subMetro myM l =
			match myM with
				V(n) -> (List.mem n l)
			|	P(n, subm) -> (subMetro subm (n::l))
			|	C(m1, m2) -> ((subMetro m1 l) && (subMetro m2 l))
		in
		(subMetro m [])
