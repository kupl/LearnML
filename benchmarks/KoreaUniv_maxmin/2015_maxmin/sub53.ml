(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
|[x] -> x
|h::t -> if h > max t then h else max t;;

let rec min : int list -> int
=fun l -> match l with
|[x] -> x
|h::t -> if h < min t then h else min t ;;
