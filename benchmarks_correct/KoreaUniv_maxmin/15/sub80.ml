let rec max : int list -> int
=fun l -> match l with 
[] -> -1
| h::[] -> h
| h::t -> let temp = max t in if h>temp then h else temp
 