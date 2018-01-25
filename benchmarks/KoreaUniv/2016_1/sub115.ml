(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n = 0 then 1 else if n = 1 then 1 else fib (n-1) + fib (n-2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) ->  if n2 =0  then 1 else if n2 = n1 then 1  else ((pascal ((n1-1),(n2-1))) +( pascal (n1-1, (n2))))

(* Problem 3 *)
let rec is_prime : int * int -> bool
= fun (n1, n2) -> if (n2 = 1) then true else if (n1 mod n2 = 0) then false  else is_prime (n1, n2-1) 

let rec prime : int -> bool
= fun n -> if n = 0 then false else if n = 1 then true else if n = 2 then true else is_prime(n, n-1)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if b = a then f(b) else f(b)+sigma f a (b-1)

