(* 컴퓨터공학부 2009-11833 창배성 *)
let rec products (matrix , i , j) =
		if j = 0 then 1.0
		else (matrix (i,j)) *. (products (matrix, i, j-1))
let rec sumprod (matrix , n , k) =
	if ( n < 0 || k <0) then raise ("Invalid matrix size")
	if n =0  then 0.0
	else products (matrix ,n ,k) +. sumprod (matrix , n-1 , k)