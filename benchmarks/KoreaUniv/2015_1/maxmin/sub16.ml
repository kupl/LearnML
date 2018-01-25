let rec max : int list -> int
=fun l -> match l with
[] -> min_int
|h::t -> if max t > h then max t else h;;

let rec min : int list -> int
=fun l -> match l with
[] -> max_int
|h::t -> if min t < h then min t else h;;
