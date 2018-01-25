exception InputError

(* Problem 1 *)
let rec fib : int -> int
= fun n -> 
  if n < 0 then raise InputError
  else if n = 0 then 0
  else if n = 1 then 1
  else fib (n - 1) + fib (n - 2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 
  if n1 < 0 || n2 < 0 || n1 < n2 then raise InputError
  else if n2 = 0 then 1
  else if n2 = n1 then 1
  else pascal ((n1 - 1), n2) + pascal ((n1 - 1), (n2 - 1))

(* Problem 3 *)
let rec prime_sub : int -> int -> bool
= fun a b ->
  if b = 0 then false
  else if b = 1 then true
  else if (a mod b = 0) then false
  else prime_sub a (b - 1)

let rec prime : int -> bool
= fun n -> 
  if n <= 0 then raise InputError
  else prime_sub n (n - 1)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 
  if a < b then f a + sigma f (a + 1) b
  else if a = b then f a
  else raise InputError

