let sumprod (m, n, k) =
	let prod i =
		let rec f j =
			if j=k then m(i,j)
			else m(i,j)*.f (j+1)
		in
		f 1
	in
	let rec sum i =
		if i=n then prod i
		else prod i+.sum (i+1)
	in
	if n<1 || k<1 then invalid_arg "sumprod"
	else sum 1

(*
let m (i,j) = float_of_int i *. float_of_int j

let _ = print_float (sumprod(m,3,3));print_newline()
*)
