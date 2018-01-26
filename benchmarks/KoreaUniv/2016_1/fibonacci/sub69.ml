(*problem1*)
let rec fib : int -> int
=fun n->
	if n=0 || n=1 then 1
	else fib(n-1) + fib(n-2)

