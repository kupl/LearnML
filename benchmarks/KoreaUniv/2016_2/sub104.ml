let rec f : int list -> int
= fun lst ->
match lst with
|[] -> -999
| h::t-> let a=f t in if h>a then h else a;;