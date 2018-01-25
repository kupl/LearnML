(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n= 0 then 0
else if n=1 then 1
else fib (n-1) + fib (n-2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n1=n2 then 1
else if n2=0 then 1
else pascal (n1-1,n2-1)+pascal (n1-1,n2)

let rec prime2 a b
= if a=1 then false
else if a=b then true
else if (a mod b)=0 then false
else prime2 a (b+1)

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> prime2 n 2

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if b=a-1 then 0
else sigma f a (b-1) + f b

