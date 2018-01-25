(* Problem 1 *)
exception Problem

let rec fib n =
 
	if n<0 then raise Problem
	else

match n with
0 -> 0
|1 -> 1
|_ -> fib (n-1) + fib (n-2);;


(* Problem 2 *)
let rec  pascal (n1, n2) =

	if n1<0 || n2<0||n1 < n2 then raise Problem
	else

if n1==n2 then 1
else if n2==0 then 1
else pascal (n1-1, n2-1) + pascal (n1-1, n2);;


(* problem 3 *)
let prime n =

	if n<=0 then raise Problem
	else

if n == 1 then false
else 
let rec undivi d =
d>=n || n mod d <>0 && undivi (d+1) in undivi 2;;


(* Problem 4 *)
let rec sigma f a b =

	if a<0||b<0||a>b then raise Problem
	else
 
if a == b then f b

else f b + sigma f a (b-1);;



