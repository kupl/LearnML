(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n = 0 then 0 else if n = 1 || n = 2 then 1 else fib (n-1) + fib (n-2)

(* Problem 2 *)
let rec fact n = if n = 0 then 1 else if n = 1 then 1 else n * fact (n-1)

let rec pascal : int * int -> int
= fun (n1, n2) -> fact (n1) / ( fact(n2) * fact(n1 - n2))

(* Problem 3 *)
let rec checkPrime n x 
= if x * x > n then true
  else if n mod x = 0 then false
  else checkPrime n (x+1)

let rec prime : int -> bool
= fun n -> checkPrime n 2

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a = b then f(a) else f(a) + sigma f (a+1) b

