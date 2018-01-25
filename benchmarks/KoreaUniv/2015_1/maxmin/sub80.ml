let rec max : int list -> int
=fun l -> match l with 
[] -> -1
| h::[] -> h
| h::t -> let temp = max t in if h>temp then h else temp

let rec min : int list -> int
=fun l -> match l with 
[] -> -1
| h::[] -> h
 | h::t -> let temp = min t in if h<temp then h else temp
