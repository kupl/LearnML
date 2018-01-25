(* Problem 1 *)
let rec fib : int -> int
= fun n -> match n with 
   0 -> 0
  |1 -> 1
  |_ -> fib (n-1) + fib (n-2);; 

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> (if n2 = 0 || n1 = n2 then 1
  else pascal (n1-1,n2-1) + pascal (n1-1,n2));;

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> let rec divide d = d*d > n || (n mod d != 0 && divide (d+1))
	in n != 1 && divide 2 ;;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> (if a > b then 0 else sigma f a (b-1) + f b);;

