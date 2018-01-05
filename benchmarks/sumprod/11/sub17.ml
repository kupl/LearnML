(* 2009-11674 ±è¿øÁø HW1-3 *)

let sumprod(matrix, n, k) =
	
	let rec mul(matrix, x, y, k) res =
		if (y=k) then res
		else mul(matrix, x, (y+1), k) (res *. matrix(x,y))
	in

	let rec add(matrix, x, n, k) sum =
		if (x=n) then sum
		else add(matrix, (x+1), n, k) (sum +. (mul(matrix, x, 1, k) 1.0))
	in

	add(matrix, 1, n, k) 0.0
