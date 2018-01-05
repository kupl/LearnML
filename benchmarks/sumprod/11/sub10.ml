(* HW 1-3 / 2007-11603 / 컴퓨터공학부 / 이영준 *)

let sumprod (matrix, n, k) =
	let rec prod (i, j, x) =
		if j = k then
			x
		else
			x *. (matrix (i, (j+1)))
	in

	let rec sum (i, p) = 
		if i = n then
			p
		else
			p +. (prod ((i+1), 1, (matrix ((i+1), 1))))
	in

	sum (1, prod (1, 1, matrix(1,1)))