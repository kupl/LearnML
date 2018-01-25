(* Problem 1 *)
let rec fib : int -> int
= fun n -> if(n=1||n=2) then n-1 else fib(n-1)+fib(n-2);; (* TODO *)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if (n2=0||n1=n2) then 1 else pascal((n1)-1,(n2)-1)
+ pascal ((n1)-1,n2);; (* TODO *)

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> if n=1 then false else let rec divide m
= if n=m then true else if (n mod m) = 0 then false else divide (m+1)
in divide 2;; (* TODO *)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if(a=b) then f a else (f a) + sigma f (a+1) b ;; (* TODO *)

