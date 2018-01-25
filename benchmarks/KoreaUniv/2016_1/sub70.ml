(* Name : Jungwon Seo / Student ID : 2012210051 *)

exception Problem;;

(* Problem 1 *)
let rec fib : int -> int
= fun n ->
if n<0 then raise Problem
else if n=0 then 0
else if n=1 then 1
else fib(n-1) + fib (n-2);;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) ->
if n1<0 || n2<0 || n1<n2 then raise Problem
else if n2=0 || n1=n2 then 1
else pascal(n1-1, n2-1) + pascal(n1-1, n2);;

(* Problem 3 *)
let rec div : int * int -> bool
= fun (n, a) ->
if a<=0 then raise Problem
else if n<=1 then false
else if a=n-1 || (n=2 && a=2) then true else (n mod a <> 0) && div(n, a+1);;

let rec prime : int -> bool
= fun n ->
if div(n, 2) then true else false;;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
if a>b then raise Problem
else if a=b then f a
else f b  + sigma f a (b-1) ;;
