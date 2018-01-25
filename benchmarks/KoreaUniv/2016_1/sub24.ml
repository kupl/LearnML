(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n=1 then 0 else if n=2 then 1 else  fib(n-1)+fib(n-2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n2 = 0 then 1 else if n1=n2 then 1 else pascal(n1-1, n2) + pascal(n1-1, n2-1);;

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> let rec r :  int * int -> int = fun(n, k) -> if n = 1 then 1 else if n = 2 then 0 else if n mod k = 0 then 1 else if k = 2 then 0 else r(n, k-1) in if r(n, n-1) = 0 then true else false;; 

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a>b then 0 else sigma f (a+1) b + f(a);;


