(* Problem 1 *)
let rec fibonacci : int -> int
= fun n -> match n with
| 0 -> 0
| 1 -> 1
| n -> fibonacci(n-1) + fibonacci(n-2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> match n2 with
| 0 -> 1
| n2 -> if n1==n2 then 1 else pascal(n1-1,n2-1)+pascal(n1-1,n2)

(* Problem 3 *)
let rec prime = fun n -> 
let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
    n <> 1 && is_not_divisor 2

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a==b then f a else sigma f (a+1) b  +f(a)
