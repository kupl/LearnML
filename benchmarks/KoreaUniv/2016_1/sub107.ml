(* Problem 1 *)
let rec fib : int -> int 
= fun n -> 0 

let rec fib n = if n<0 then raise (Failure "n cannot be negative") 
else if (n=0)||(n=1) then n else fib(n-1) + fib(n-2);;
(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 0 
 
let rec pascal (x,y) = if (x<0)||(y<0) then raise (Failure "inserted numbers cannot be negative")
else if x<y then raise (Failure "first value cannot be smaller than second value") 
else if x=y then 1
else if y=0 then 1
else pascal (x-1,y-1) + pascal (x-1,y);;

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> true 

let rec prime n = let rec dividing d =
if n mod d = 0 then false
else if d*d > n then true  else dividing (d+1) in
if n<=0 then raise (Failure "the number should be positive")
else if n=1 then false
else if n=2 then true 
else dividing 2;; 

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 0 

let rec sigma f a b = if a>b then (f a) + (sigma f (a-1) b)
else if (a=b) then f a
else (f a) + (sigma f (a+1)  b); 

