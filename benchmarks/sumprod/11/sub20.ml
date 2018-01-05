(* 2009-11718 1-3 *)
(*let matrix (i,j) = i+j*)

let rec sumprod (matrix, n, k) =
	let rec prod (matrix1, i, j) =
		if j<1 then raise (Invalid_argument "error")
		else if j>1 then
		matrix1 (i, j) *.prod (matrix1, i,(j-1))
		else 
		matrix1 (i, j)		
		 in
	if n<1 then raise (Invalid_argument "error")
	else if n>1 then
	prod (matrix, n, k) +. sumprod (matrix,(n-1),k)
	else
	prod (matrix, n, k)
	

(*
let testfun ((x:int), (y:int)) =
	float_of_int(x) +. float_of_int(y)*)
