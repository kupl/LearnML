(* Problem 1 *)
let rec fib : int -> int
= fun n -> 0 (* TODO *)
let rec fib n = match n with 0->0 | 1->1 | _->fib (n-1) + fib (n-2);;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 0 (* TODO *)
let rec pascal (a,b) = if (b = 0) || (a = b) then 
1 else
pascal (a-1,b-1) + pascal (a-1,b);;

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> true (* TODO *)

let rec calc x n =
if (x mod n) <> 0 then
 match n with 2 -> true | _-> calc x (n-1)
else false;;

let rec prime n = match n with 0 | 1->false | 2->true | _-> calc n (n-1)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 0 (* TODO *)
let rec sigma func a n = if (n <= a) then func a else func n + sigma func a (n-1);;

