(* Problem 1 *)
let rec fib n = if n = 0 then 0 
	else if n = 1 then 1 
	else fib (n-2) + fib (n-1)

(* Problem 2 *)
let rec pascal (n1,n2) = if n2 = 0 then 1
	else if n2 = n1 then 1
	else pascal (n1-1,n2-1) + pascal (n1-1,n2)

(* Problem 3 *)
let prime n =
	if n = 0 || n = 1 then false
	else if n = 2 then true
	else let rec isprime a = if a*a > n then true
			else (n mod a) <> 0 && isprime (a+1)
		in isprime 2
		 

(* Problem 4 *)
let rec sigma f a b = 
	if a > b then 0
	else if a = b then f b
	else f a + sigma f (a+1) b

