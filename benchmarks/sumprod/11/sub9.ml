exception OutOfBound
(* 곱을 계산하기 위한 함수 *)
let rec prod(matrix, n, k) = 
	if k>1 then matrix(n, k) *. prod(matrix, n, k-1)
	else if k=1 then matrix(n, k)
	else raise OutOfBound

let rec sumprod(matrix, n, k) = 
	if n>1 then prod(matrix, n, k) +. sumprod(matrix, n-1, k)
	else if n=1 then prod(matrix, n, k)
	else raise OutOfBound
