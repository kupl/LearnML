(* Problem 3 *)
let rec max : int list -> int
= fun l -> match l with
[]-> 0
|h::t -> if(t=[]) then h 
else if(h>max t) then h else max t

 