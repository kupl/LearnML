(* Name : Jungwon Seo / Student ID : 2012210051 *)

exception Problem;;
let rec pascal : int * int -> int
= fun (n1, n2) ->
if n1<0 || n2<0 || n1<n2 then raise Problem
else if n2=0 || n1=n2 then 1
else pascal(n1-1, n2-1) + pascal(n1-1, n2);;
