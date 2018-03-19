type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec apply e n =
match e with
	|X -> n
	|INT i -> i
	|ADD (e1,e2) -> apply e1 n + apply e2 n
	|SUB (e1,e2) -> apply e1 n - apply e2 n
	|MUL (e1,e2) -> apply e1 n * apply e2 n
	|DIV (e1,e2) -> apply e1 n / apply e2 n 
	|SIGMA (e1,e2,e3) -> 
	let rec sigma a b e3 =
			if( a > b+1) then raise (Failure "Error!")
			else if (a <= b)
			then (apply e3 a)+(sigma (a+1) b e3) 
			else 0 in 
			sigma (apply e1 n) (apply e2 n) e3

let calculator : exp -> int
=fun e -> apply e 1

 