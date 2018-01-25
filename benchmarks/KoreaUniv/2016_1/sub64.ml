(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n=0 then 0
else if n=1 then 1
else fib(n-1) + fib(n-2) (* TODO *)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n2=0 then 1
else if n1=n2 then 1
else pascal(n1-1, n2-1) + pascal(n1-1,n2) (* TODO *)

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> if n<=1 then false
else if n<=3 || n=5 || n=7  then true
else if (n mod 2=0) ||  (n mod 3=0) then false
else prime(n-6)
 (* TODO *)

(* Problem 4 *)

let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a=b then f a
else f b + sigma(f) a (b-1) (* TODO *)
