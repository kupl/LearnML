let rec max : int list -> int
=fun l -> match l with
[] -> 0
| h::t -> if h> max t then h
else if (max t)=0&&(t=[]) then h
else max t

let rec min : int list -> int
=fun l -> match l with
[] -> 0
| h::t -> if h< min t then h
else if (min t)=0&&(t=[]) then h
else min t
