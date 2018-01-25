(*problem1*)
let rec fib : int -> int
=fun n->
	if n=0 || n=1 then 1
	else fib(n-1) + fib(n-2)


(*problem2*)
let rec pascal : int * int -> int
= fun(x,y) -> 
	if x=0 || y=x then 1
	else pascal(x-1,y-1) + pascal(x-1,y)

(*problem3*)
let rec prime n
=let rec help d
	=d*d>n || (n mod d<>0 && help(d+1)) in n<>1 && help 2
	


(*problem4*)
let rec sigma : (int->int) -> int -> int -> int
=fun f a b ->
	if a=b then (f a)
	else (f a) + (sigma f (a+1) b);;
