(* Problem 1 *)

let rec fib = fun n ->  if n = 1 then 0
else if n = 2 then 1
else fib (n-1) + fib (n-2)

(* Problem 2 *)

let rec pascal= fun (a,b) ->
if(b=0) then 1
else if (a=b) then 1
else pascal (a-1,b-1) + pascal (a-1, b)

(* Problem 3 *)

let rec prime n =
if n = 0 || n = 1 then false
else if n = 2 then true
else let rec prime_1 x = if x*x > n then true
else (n mod x) <> 0 && prime_1 (x+1)
in prime_1 2
(* Problem 4 *)
let rec sigma f a b = 
	if a = b then f b
	else f a + sigma f (a+1) b
