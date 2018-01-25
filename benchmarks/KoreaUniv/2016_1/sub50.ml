(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n = 0 then 0 else if n = 1 then 1 else fib(n-1) + fib(n-2);;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n1 = n2 || n2 = 0 then 1 else pascal(n1-1,n2-1) + pascal(n1-1,n2);;

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> let k = int_of_float (floor (sqrt (float_of_int n))) in
let rec modif n k =
if k = 1 then true
else if k >= 2 && n mod k = 0 then false
else if k = 2 && n mod k <> 0 then true
else modif n (k-1) in
if n >= 2 then modif n k
else false;;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a > b then 0 else (f b) + sigma f a (b-1);;

