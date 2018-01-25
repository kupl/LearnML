(* Problem 1 *)

let rec fib : int->int =fun n ->
if n=0 then 0
else if n<0 then -1
else if n=1 then 1
else fib(n-1) + fib(n-2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) ->
 if n2=0 then 1
 else if n1=n2 then 1 
else if n1<0 then -1
else if n2<0 then -1
else pascal((n1-1),(n2-1))+pascal((n1-1),(n2)) (* TODO *)

(* Problem 3 *)	
let rec prime : int -> bool
= fun n ->
let rec divisible a =
(n>a)&&(n mod a = 0 || divisible (a+1)) in
if n=1 then false
else if n<1 then false
else if n=2 then true
else if divisible(2) then false
else true

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 
if a>b then f(a) + sigma f b (a-1)
else if a=b then f(b)
else f(b) + sigma f a (b-1)
 (* TODO *)