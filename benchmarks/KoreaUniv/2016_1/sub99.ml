(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n < 2 then 1 else fib(n-1)+fib(n-2);;
(* TODO *)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n1 = n2 then 1
            else if n2 = 0 then 1
            else pascal(n1-1,n2-1) + pascal(n1-1,n2);; 
(* TODO *)

(* Problem 3 *)
let rec prime : int -> bool
= fun n-> for i = 2 to n-1
            if n mod i = 0 then false
          else then true;;
(* TODO *)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> sigma f b - sigma f a-1;;
(* TODO *)

