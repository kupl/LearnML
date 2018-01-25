(* Problem 1 *)
let rec fib : int -> int
= fun n -> 0 (* TODO *)

let rec fib : int -> int
= fun n -> 
if n = 0 then 1
else if n = 1 then 1
else (n * fib (n-1))

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 0 (* TODO *)

let rec pascal : int * int -> int
= fun (n1, n2) ->
if n2 = 0 then 1
else if n1 = 0 then 1
else if n1 = n2 then 1
else if n1 < n2 then raise (Failure "Invalid Input")
else (pascal (n1-1, n2) + pascal (n1-1, n2-1))

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> true (* TODO *)

let rec prime : int -> bool
= fun n ->
if n <= 1 then false
else let rec not_divisor d = (d*d > n) || (n mod d <> 0 && not_divisor (d+1)) in (not_divisor 2)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 0 (* TODO *)

let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
if a = b then (f a)
else if a > b then raise (Failure "Invalid Input")
else ((f a) + sigma f (a+1) b)