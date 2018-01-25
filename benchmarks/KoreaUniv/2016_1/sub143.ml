(* Problem 1 *)
let rec fib n =
 match n with
    0 -> 0
  | 1 -> 1
  |_ -> fib (n - 1) + fib (n - 2);;
val fib : int -> int = <fun>

(* Problem 2 *)
let rec pascal (n1, n2) =
 if n2 = 0 then 1
  | n1 = n2 then 1
  |_ pascal (n1-1, n2-1) + pascal (n1-1, n2);;
val pascal : int * int -> int = <fun>


(* Problem 3 *)
let rec prime n =
 let n = abs n in
 let rec divisor d =
 d*d > n || (n mod d <> 0 && divisor (d+1)) in
 n <> 1 && divisor 2;;
val prime : int -> bool = <fun>


(* Problem 4 *)
let rec sigma f = fuction
 if b = a -> fun a
 |_ sigma f a (b - 1) + fun b;;
val sigma : (int -> int) -> int -> int -> int = <fun>

