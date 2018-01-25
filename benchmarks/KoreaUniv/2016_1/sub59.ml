(* Problem 1 *)
let rec fib : int -> int
= fun n -> match n with
| 0 -> 0
| 1 -> 1
| n -> fib(n-1) + fib(n-2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n2 =0 then 1
else if n1 = n2 then 1
else pascal(n1-1,n2-1) + pascal(n1-1,n2)

(* Problem 3 *)
let rec pr = fun (n,i) -> if n mod i =0  then false
else if i = 2 then true
else pr(n,i-1)
let rec prime : int -> bool
= fun n -> if n=2 || n=3 then true
else pr (n, n/2)


(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a > b then 0
else f a + sigma f (a+1) b
