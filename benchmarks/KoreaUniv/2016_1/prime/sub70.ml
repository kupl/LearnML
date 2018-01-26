(* Name : Jungwon Seo / Student ID : 2012210051 *)

exception Problem;;

let rec div : int * int -> bool
= fun (n, a) ->
if a<=0 then raise Problem
else if n<=1 then false
else if a=n-1 || (n=2 && a=2) then true else (n mod a <> 0) && div(n, a+1);;

let rec prime : int -> bool
= fun n ->
if div(n, 2) then true else false;;
