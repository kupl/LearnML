let rec max : int list -> int
=fun l -> match l with
| [a] -> a
| h::t -> let m = max t in
if(h>m) then h
else m;;

let rec min : int list -> int
=fun l -> match l with
| [a] -> a
| h::t -> let m = min t in
if(h<m) then h
else m;;
