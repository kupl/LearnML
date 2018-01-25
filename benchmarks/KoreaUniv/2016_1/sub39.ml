(* Problem 1 *)
let rec fib n =
	if n > 2 then fib(n-1) + fib(n-2) 
	else 1;;
fib 3;;

(* Problem 2 *)
let rec pascal (a, b) =
	if b = 0 then 1
	else if b =  a then 1
	else (pascal ((a-1), (b-1))) + (pascal ((a-1), b));;
pascal (4, 2);;

(* Problem 3 *)
let rec prime n =
	let rec div d = 
		d * d > n || (n mod d <> 0 && div(d+1)) in
		n <> 1 && div 2;;

(* Problem 4 *)
let rec sigma f x y =
	if x = y then f y
	else sigma f (x+1) y + f x;;
