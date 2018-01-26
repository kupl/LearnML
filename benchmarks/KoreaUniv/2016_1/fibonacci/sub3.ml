(* Problem 1 *)
exception Integer_overflow_problem

let rec calc_fib : int * int * int -> int
= fun (n, prev1, prev2) ->
	if n>0 then
		if n<=2 then
			prev1+prev2
		else
			calc_fib (n-1, prev2, prev1+prev2)
	
	else
		if n>=(-2) then
			prev1-prev2
		else
			calc_fib (n+1, prev2, prev1-prev2)

let rec fib : int -> int 
= fun n ->
	match n with
		0 -> 0
		|(-1)|1 -> 1
		|_ -> 
			if n>90 || n<(-90) then	(* limit of integer.... to prevent integer overflow *)
				raise Integer_overflow_problem
			else
				calc_fib (n, 0, 1)
