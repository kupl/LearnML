(* Problem 1 *)
let rec fib n=
if n>=2 then fib(n-1)+fib(n-2)
else if n=1 then 1
else 0;;

(* Problem 2 *)
 let rec pascal (n1, n2) =
   let x = n1 = n2 in
   match (n1, n2) with
   (n1, 0) -> 1
   |x -> 1
   |_ -> pascal (n1 -1, n2 - 1) + pascal (n1 -1, n2);;

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> true (* TODO *)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 0 (* TODO *)

