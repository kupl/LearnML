(* Problem 1 *)
let rec fib : int -> int
= fun n -> 0 (* TODO *)

# let rec fib n = 
match n with
0 -> 0 | 1 -> 1 | _ -> (fib(n-1)) + (fib(n-2));;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 0 (* TODO *)

# let rec pascal (a, b) = 
if b = 0 then 1
else if a = b then 1
else pascal(a-1, b-1) + pascal(a-1, b);;

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> true (* TODO *)

# let rec prcheck (n, a) =
if n = a then true
else if (n mod a) = 0 then false
else prcheck(n, a+1);

# let rec prime n =
if n = 1 || n = 2 then true
else prcheck(n, 2);;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 0 (* TODO *)

# let rec sigma f a b =
if a = b then f a
else f a + sigma f (a + 1) b;;