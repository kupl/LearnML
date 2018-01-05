(* 4 sumprod : (int * int -> real) * int * int -> real *)
let rec sumprod (m, n, k) =
	let rec prod (m, i, k) =
		if k <= 0 then 1.0
			  else m (i, k) *. (prod (m, i, k-1)) in
	let rec sum (m, n, k) =
		if n <= 0 then 0.0
			  else prod (m, n, k) +. sum (m, n-1, k) in
	if n < 1 or k < 1 then 0.0
			  else sum (m, n, k)

