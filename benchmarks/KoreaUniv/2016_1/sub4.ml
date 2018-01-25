(* Problem 1 *)
let rec fib (n : int) : int =
match n with
0 -> 0
|1 -> 1
|_ -> fib (n - 1) + fib (n - 2);;

(* Problem 2 *)
let rec pascal (x, y) =
if x < 0 || y < 0 then 0
else if x = 0 && y = 0 then 1
else pascal (x - 1, y - 1) + pascal (x - 1, y);;

(* Problem 3 *)

(* helper function *)
let rec testPrime(i, n) =
if i * i > n then true
else if n mod i = 0 then false
else testPrime(i + 1, n);;


let rec prime n = testPrime(2, n);;

(* Problem 4 *)
let rec sigma (tmp : int -> int) a b =
if a > b then 0
else tmp a + sigma tmp (a + 1) b;;