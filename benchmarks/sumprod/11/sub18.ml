(* 컴퓨터공학부/2009-11679/김정명/3 *)

let rec sumprod (mtx, n, k) =
	let rec prod (i, j) =
		if j = 1 then mtx (i, j)
		else mtx (i, j) *. prod (i, j-1)
	in
	if (n < 1 || k < 1) then raise (Invalid_argument "sumprod")
	else if n = 1 then prod (n, k)
	else prod (n, k) +. sumprod(mtx, n-1, k)


(* let matrix (i, j) = float (i + j) *)

