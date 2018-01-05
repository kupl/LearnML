(* C:\Users\owner\Desktop\Homework 1(2).ml *)

exception Error of string;;

(* Syntax Error or Unbound Value
let sumprod(matrix, n, k) = 

	let rec prod(i, j, matrix) =
		if (j == 1) then matrix(i, j)
		else matrix(i, j) * prod(i, (j - 1), matrix) in

	if (n <= 0) || (k >= 0) then raise(Error "FAIL!")
	else if n == 1 then prod(n, k, matrix)
	else prod(n, k, matrix) + sumprod(matrix, (n - 1), k) ;;
 *)

(* Syntax Error or Unbound Value
let sumprod(matrix, n, k) = 

	let rec prod(i, j, matrix) =
		if (j == 1) then matrix(i, j)
		else matrix(i, j) *. prod(i, (j - 1), matrix) in

	if (n <= 0) || (k >= 0) then raise(Error "FAIL!")
	else if n == 1 then prod(n, k, matrix)
	else prod(n, k, matrix) +. sumprod(matrix, (n - 1), k) ;;
 *)

(* Syntax Error or Unbound Value
let sumprod(matrix, n, k) = 

	let rec prod(i, j, matrix) =
		if (j == 1) then matrix(i, j)
		else matrix(i, j) *. prod(i, (j - 1), matrix) in

	if (n <= 0) || (k >= 0) then raise(Error "FAIL!")
	else if n == 1 then prod(n, k, matrix)
	else prod(n, k, matrix) +. sumprod(matrix, (n - 1), k) ;;
 *)

(* Syntax Error or Unbound Value
let sumprod(matrix, n, k) = 

	let rec prod(i, j, matrix) =
		if (j == 1) then matrix(i, j)
		else matrix(i, j) *. prod(i, (j - 1), matrix) in

	if (n <= 0) || (k >= 0) then raise(Error "FAIL!")
	else if n == 1 then prod(n, k, matrix)
	else prod(n, k, matrix) +. sumprod(matrix, (n - 1), k) ;;
 *)

let rec sumprod(matrix, n, k) = 

	let rec prod(i, j, matrix) =
		if (j == 1) then matrix(i, j)
		else matrix(i, j) *. prod(i, (j - 1), matrix) in

	if (n <= 0) || (k >= 0) then raise(Error "FAIL!")
	else if n == 1 then prod(n, k, matrix)
	else prod(n, k, matrix) +. sumprod(matrix, (n - 1), k) ;;

