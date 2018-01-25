(* Problem 1 *)
let rec fib : int -> int
= fun n ->  (* TODO *)
match n with
0->0
|1->1
|_->fib (n-1) + fib (n-2);;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) ->  (* TODO *)
if n2=0 then 1
else if n1=n2 then 1
else pascal (n1-1,n2) + pascal (n1-1,n2-1);;

(* Problem 3 *)
let prime : int -> bool
= fun n ->  (* TODO *)
let rec prime2(n,x) = 
if x=n then true
else if n mod x=0 then false
else prime2 (n,x+1)
in prime2(n,2);;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->  (* TODO *)
let rec sigma2 f n s =
if n<=b then sigma2 f (n+1) (s+f n)
else s
in sigma2 f a 0;;
