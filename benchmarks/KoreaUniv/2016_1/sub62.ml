(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n < 0 then (-9999999) else if n = 0 then 0 else if n = 1 then 1 else fib(n - 1) + fib(n - 2) 

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n1 < 0 || n2 < 0 || n2 > n1 then (-9999999) else  if n2 = 0 || n1 = n2 then 1 else pascal(n1 - 1,n2 - 1) + pascal(n1-1,n2)

(* Problem 3 *)
let rec prime : int -> bool
= fun n ->
let rec isprime : int -> int ->bool
= fun n a -> if a >= n then true else if a < n && n mod a = 0 then false else isprime n (a + 1) in
if n <= 1 then false else isprime n 2

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a <= b then f a + sigma f (a+1) b else 0
