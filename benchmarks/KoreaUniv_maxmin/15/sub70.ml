let rec max : int list -> int
=fun l -> match l with 
    |[]->0
    |h::[]->h
    |h::t-> if max t< h then h else max t;;

let rec min : int list -> int
=fun l -> match l with 
    |[]->0
    |h::[]->h
    |h::t-> if min t> h then h else min t;;
