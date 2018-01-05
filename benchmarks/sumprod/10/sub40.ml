(* 4190.310 Programming Language		*
 * Homework #1 - Exercise 2 (합곱)		*
 * 2008-11744 Jongwook Choi 			*)

exception Error of string
type real = float

let sumprod( matrix, n, k ) =
	if n <= 0 or k <= 0 then 
		raise (Error "Assertion 'n > 0 and k > 0' failed!")
	else
	let rec accumulate(init, a, b, oper, f) =
		if a > b then init
		else oper(f(a), accumulate(init, a+1, b, oper, f))
	in
	let multiplication = fun (x,y) -> x*.y in
	let addition = fun(x,y) -> x+.y in
	accumulate(0.0, 1, n, addition, fun i -> 
			accumulate(1.0, 1, k, multiplication, fun j -> matrix(i, j))
		)


