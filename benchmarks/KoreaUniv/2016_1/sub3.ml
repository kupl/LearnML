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

(* Problem 2 *)
let round : float -> int
= fun f ->
	int_of_float (floor (f +. 0.5))

let rec calc_pascal_row : int * int * int -> float
= fun (n1, n2, i) ->
	if n2=i then
		1.
	else
		calc_pascal_row (n1, n2, i+1)*.(float_of_int (n1-i))/.(float_of_int (i+1))

let rec pascal : int * int -> int
= fun (n1, n2) -> 
	if n2>n1 || n1<0 || n2<0 then
		0
	else
		if n1=n2 || n2=0 then
			1
		else
			if n1/2<n2 then
				if int_of_float (calc_pascal_row (n1, n1-n2, 0))=0 then
					raise Integer_overflow_problem
				else
					round(calc_pascal_row (n1, n1-n2, 0))
			else
				if int_of_float (calc_pascal_row (n1, n2, 0))=0 then
					raise Integer_overflow_problem
				else
					round(calc_pascal_row (n1, n2, 0))

(* Problem 3 *)
let rec check_prime : int * int -> bool
= fun (target, quotient) ->
	if target<=1 then
		false
	else
		if target mod quotient>0 then
			if target/2<quotient then
				true
			else
				if quotient>2 && quotient mod 2=1 then
					check_prime (target, quotient+2)
				else
					check_prime (target, quotient+1)
		else
			if target=quotient then
				true
			else
				false

let rec prime : int -> bool
= fun n ->
	check_prime (n, 2)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
	if a>b then
		0
	else
		f b + sigma f a (b-1)
