let rec multi : int list -> int -> int
= fun l r ->
match l with
  |[] -> r
  |hd :: tl -> multi tl (10*r + hd);;

let rec lst2int : int list -> int
= fun lst -> (*TODO*)
match lst with
  |[] -> 0
  |hd :: tl -> multi tl hd;;


lst2int [2;3;4;5];;