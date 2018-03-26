let rec max : int list -> int
=fun l->
  match l with
  [] -> (min_int)
  |h::t -> if h>max t then h else max t;;

let rec min : int list -> int
=fun l ->
  match l with
  [] -> (max_int)
  |h::t -> if h<min t then h else min t;;
