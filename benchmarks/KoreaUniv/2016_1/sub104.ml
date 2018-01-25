(* Problem 1 *)
let rec fib (n : int) =
 match n with
 0 -> 0 | 1 -> 1 | _ -> fib (n-2) + fib (n-1);;

(* Problem 2 *)
let rec pascal (n1, n2) =
 if n2 = 0 || n1 = n2 then 1
 else pascal (n1-1, n2-1) + pascal (n1-1, n2);;

(* Problem 3 *)
let rec prime_iter a b =
 if a <= b then true
 else if a mod b = 0 then false
 else prime_iter a (b+2);;

let prime (n: int) =
 if n < 2 then false
 else if n = 2 then true
 else if n mod 2 = 0 then false
 else prime_iter n 3;;

(* Problem 4 *)  
let rec sigma (f : int -> int) (a : int) (b : int) =
 if a = b then f a
 else f a + sigma f (a+1) b;;
