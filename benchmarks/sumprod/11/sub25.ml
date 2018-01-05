(* 2009-11824 Jieun-Jeong HW1-3 *)

let rec sumprod (matrix, n, k) =
	let rec prod (matrix, i, k) =	(* prod : i번째 행의 원소를 다 곱하는 함수 *)  
		if k == 1 then matrix (i, k)
		else (matrix (i, k)) *. (prod (matrix, i, (k-1))) in
	if n <= 0 then raise (Invalid_argument "n is positive")
	else if k <= 0 then raise (Invalid_argument "k is positive")
	else if n == 1 then prod (matrix, n, k)
	else (prod (matrix, n, k)) +. (sumprod (matrix, (n-1), k))
(*
let matrix (i, j) =
	if i == 1 then
		if j == 1 then 1.0
		else if j == 2 then 2.0
		else 3.0
	else if i == 2 then
		if j == 1 then 4.0
		else if j == 2 then 5.0
		else 6.0
	else 
		if j == 1 then 7.0
		else if j ==2 then 8.0
		else 9.0
*)
