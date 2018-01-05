(* 2008-11874 Lee, Sujee *)
(* EXERCISE 3 *)

let rec sumprod(matrix, n, k) = (* sumprod : (int * int -> float) * int * int -> float = <fun> *)
	let rec prodloop(matrix, n, k) =
		if k=1 then matrix(n,1)
		else if k>1 then matrix(n,k) *. prodloop(matrix, n, k-1)
		else 0.0
	in 
	if n=1 then prodloop(matrix, n, k)
	else if n>1 then prodloop(matrix, n, k) +. sumprod(matrix, n-1, k)
	else 0.0
	(* *., +. : float -> float -> float *)
	
(*
let mat(a,b) = 2.5
let result3 = sumprod(mat, 2,3)

let _ =
	print_string "EXERCISE 3 : ";
	print_float result3;
	print_newline()
	*)