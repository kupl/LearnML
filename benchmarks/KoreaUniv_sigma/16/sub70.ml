(* Name : Jungwon Seo / Student ID : 2012210051 *)

exception Problem;;

let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
if a>b then raise Problem
else if a=b then f a
else f b  + sigma f a (b-1) ;;
