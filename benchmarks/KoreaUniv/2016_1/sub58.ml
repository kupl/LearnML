(* Problem 1 *)
let rec fib : int -> int
= fun n ->
(match n with
0 -> 0
|1 -> 1
|a -> fib (a-1) + fib (a-2) )

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) ->
if n2 = 0 || n1 = n2 then 1
else pascal ((n1 - 1), (n2 - 1)) + pascal ((n1 - 1), n2)

let rec primeTest n i =
if i = 1 then true
else if (n mod i) = 0 then false
else primeTest n (i - 1)


(* Problem 3 *)
let rec prime : int -> bool
= fun n ->
if n < 2 then false
else primeTest n (n - 1)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
if a > b then 0
else if a = b then f a
else f a + sigma f (a + 1) b

