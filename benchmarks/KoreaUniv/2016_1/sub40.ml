(* Problem 1 *)
let rec fib : int -> int = fun n -> if n==0 then 0 else if n==1 then 1 else fib n-1 + fib n-2;;

(* Problem 2 *)
let rec pascal : int * int -> int = fun (n1, n2) -> if n1==0||n2==0 then 1 else if n1==n2 then 1 else pascal (n1-1,n2-1)+pascal (n1-1,n2);;

(* Problem 3 *)
let rec prime : int -> bool = fun n -> let rec isprime(n,x) = if x==1 then true else if n mod x==0 then false else isprime(n,x-1) in if n<2 then false else isprime(n,n-1);;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int = fun f a b -> if a==b then f(a) else sigma f a (b-1) + f(b);;

