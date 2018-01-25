(* Problem 1 *)
let rec fib n =
	if n < 2 then n else fib(n-1) + fib(n-2);;
	


(* Problem 2 *)
let rec pascal (a, b) =
	if b = 0 then 1
	else if b = a then 1
	else pascal (a-1, b-1) + pascal (a-1, b);;
	

(* Problem 3 *)
 let rec is_prime ?(m=2) n =
    match n with 
	| 1 -> false | _ -> 
    m * m > n || n mod m <> 0 && is_prime ~m:(m+1) n;;
  
 	
(* Problem 4 *)
let rec sigma f a b =
  if a > b then 0 
  else (f a) + sigma f (a+1) b;;