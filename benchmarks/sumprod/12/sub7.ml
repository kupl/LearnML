
let rec sumprod(matrix, n, k) =
	if n < 1 then 0.
	else if k < 1 then 0.
	else
		let rec prod (i, j) =
			if j < 1 then 1.
			else matrix(i,j) *. (prod (i, j-1))
		in
		(prod (n, k)) +. sumprod(matrix, n-1, k)
	;;

(* exercise test
Printf.printf "sumprod : %f\n" (sumprod((fun (x,y) -> float_of_int(2*x-y)), 2, 3));;
exercise *)
