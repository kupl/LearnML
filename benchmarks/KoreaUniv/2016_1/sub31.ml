(* Problem 1 *)
let rec fib n =
 match n with 
| 0 -> 0
| 1 -> 1
| _ -> fib(n-1) + fib(n-2)

(* Problem 2 *)
let rec pascal(n1, n2) = 
 match (n1, n2) with 
 | (n1, 0) -> 1
 | (1, 1) -> 1
 | _ ->
 if n1=n2 then 1
 else pascal(n1-1, n2-1) + pascal(n1-1, n2)

(* Problem 3 *)
let rec prime n =
	let rec divisor d = 
	d > n/2 || (divisor (d+1) && n mod d <> 0) in 
  n <> 1 && divisor 2

(* Problem 4*)
let rec sigma f a b =
 if a > b then 0
 else
 let result = f a in
 result + sigma f (a+1) b
