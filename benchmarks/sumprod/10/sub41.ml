(* complete *)
(*
1 3 2
4 1 0
0 2 3
*)
exception Invalid_Input

let sumprod(matrix,n,k) =
	let rec mul i j =
		if j = k then matrix(i,j)
			else (matrix(i,j)) *. (mul i (j+1))
	in
	let rec sum i =
		if i = n then mul i 1
			else (mul i 1) +. (sum (i+1))
	in
	if ((n <= 0)||(k <= 0)) then raise Invalid_Input
		else sum 1
;;
