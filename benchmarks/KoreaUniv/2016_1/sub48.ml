(*problem 1*)
let rec fib : int -> int
= fun n ->
if n<0 then -1
else if n=0 then 0
else if n=1 then 1
else fib (n-1) + fib (n-2);;

(*problem2*)
let rec pascal : int * int -> int
= fun (n1, n2) ->
if (n1<0 || n2<0 || n1<n2 ) then -1
else if (n1=n2 || n2=0) then 1
else pascal (n1-1, n2-1) + pascal (n1-1, n2);;

(*problem3*)
let rec prime : int -> bool
= fun n->
let rec is_not_divisor d =
	d*d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
 n<>1 && is_not_divisor 2;;

(*problem4*)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b->
if a=b then f(a)
else f(b) + sigma f a (b-1);;

