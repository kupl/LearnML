(* Problem 1 *)
let rec fib : int -> int
= fun n -> 0

let rec fib n = if (n=0 || n=1) then n
else fib(n-1) + fib(n-2);;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 0

let rec pascal (n1, n2) = if n2 = 0 then 1
else if n1 = n2 then 1
else if n2 > n1 then raise (Failure "Error")
else pascal(n1-1, n2-1) + pascal(n1-1, n2);;

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> true
let rec prime n = let rec primein (num, i) =
if i = 1 then 1
else if i = 0 then 0
else 
if (num mod i) = 0 then 0 else primein (n, i-1)
in if primein (n, n/2) = 1 then true
else if n < 0 then raise (Failure "Error")
else false;;  

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 0

let rec sigma f a b = if a > b then raise (Failure "Error")
else if a = b then (f a)
else (f a) + sigma f (a+1) b;;

