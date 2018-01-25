(* Problem 1 *)
let rec fib n = match n with 
				 0 -> 0
				|1 -> 1
				|_ -> (fib(n-1)+fib(n-2));;

(* Problem 2 *)
let rec pascal (n1, n2) = if n2=0 || n1=n2 then 1
						  else pascal(n1-1, n2-1)+pascal(n1-1, n2);;

(* Problem 3 *)
let rec prime n = if n=1 || n=2 then true
				  else god n 2;;

let rec god a b = if b=a-1 then true
				  else if a mod b=0 then false
				  else god a (b+1);;

(* Problem 4 *)
let rec sigma f a b = if a=b then f a 
					  else sigma f a+1 b + f a;;

