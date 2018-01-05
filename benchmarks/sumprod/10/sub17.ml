exception Error_of_string

let rec sumprod (mat, n, k) =
	let rec prod (mat, i, j) =
		if j=1 then mat (i, j)
		else (mat (i, j)) *. (prod (mat, i, j-1))
	in
		if (n<1) || (k<1) then raise Error_of_string
		else
		if n=1 then (prod (mat, n, k))
		else (prod (mat, n, k)) +. (sumprod (mat, n-1, k));;







(*
exception OverRange

let matrix3 (a, b) =
	match a with
		1 -> (
			match b with
				1 -> 23.1
				| 2 -> 1.0
				| 3 -> 3.4
				| _ -> raise OverRange
			)
		| 2 -> (
			match b with
				1 -> 3.1
				| 2 -> 1.3
				| 3 -> 13.0
				| _ -> raise OverRange
			)
		| 3 -> (
			match b with
				1 -> 2.0
				| 2 -> 6.0
				| 3 -> 4.0
				| _ -> raise OverRange
			)
		| _ -> raise OverRange;;
*)
