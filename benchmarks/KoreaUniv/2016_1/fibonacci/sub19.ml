let rec fib : int -> int = fun n -> 

	match n with 
	|1 -> 1
	|2 -> 1
	|_ -> fib(n-1)+fib(n-2);;
