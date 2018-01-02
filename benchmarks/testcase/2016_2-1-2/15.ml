let rec max : int list -> int
= fun lst ->
match lst with
|[] -> -999
| h::t-> let a=max t in if h>a then h else a;;

let rec f : int list -> int
= fun lst -> 
match lst with
|[] -> 999
|h::t-> let a=max t in if h<a then h else a;;
