(* Problem 1 *)
let rec fib : int -> int
= fun n -> 
match n with
0 -> 0
| 1 -> 1
| n -> fib (n-1) + fib (n-2);;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 
if n1 = n2 then 1
else if n2 = 0 then 1
else pascal((n1-1),(n2-1)) + pascal((n1-1),n2);;

(* Problem 3 *)
let rec prime : int -> bool
= fun n ->
let n = abs n in 
let rec notDivisor d =
d * d > n || (n mod d <> 0 && notDivisor (d+1)) in
n <> 1 && notDivisor 2;;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 0;; 
(*match f with
 (fun x -> x) -> (if a <= b then a + (sigma f (a+1) b) else 0)
|(fun x -> x*x) -> (if a <= b then a * (sigma f (a+1) b) else 0)
| 0;;*)
