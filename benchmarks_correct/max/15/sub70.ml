let rec max : int list -> int
=fun l -> match l with 
    |[]->0
    |h::[]->h
    |h::t-> if max t< h then h else max t;;
 