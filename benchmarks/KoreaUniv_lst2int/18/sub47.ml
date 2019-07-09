let rec lst2int : int list -> int
= fun lst -> (*TODO*)
lsttoint(reverse(lst))

and reverse : int list -> int list
=fun lst ->
  match lst with
    |[] -> []
    |h::t -> (reverse t) @ [h]

and lsttoint : int list -> int
= fun lst ->
match lst with
  |[] -> 0
  |[n] -> n
  |h::t -> h+10*(lsttoint t);;

lst2int [2;3;4;5];;
