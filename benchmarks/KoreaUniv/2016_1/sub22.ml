(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n=0||n=1 then n else fib(n-1)+fib(n-2);;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n2=0 then 1 else if n1=n2 then 1 else pascal(n1-1,n2-1)+pascal(n1-1,n2);;

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> let rec div : int-> int =fun d ->if d<n&&n mod d=0 then 0 else if d<n&&n mod d>0 then div(d+1) else if d>n||d=n then 1 else 2 in if n=2 then true else if div(2)<>0 then true else false;;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a<b then f a + sigma f (a+1) b else f a;;

