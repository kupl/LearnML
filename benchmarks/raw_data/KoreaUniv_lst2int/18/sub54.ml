let rec int_lng : int -> int
= fun a ->
  if (a/10) = 0 then 1 else 10 * int_lng (a/10);;
  
let rec lst_lng : int list -> int
= fun lst ->
  match lst with
    |[]->1
    |hd::tl -> 10*int_lng hd * lst_lng tl;;

let rec lst2int : int list -> int
= fun lst ->
  match lst with
    |[]->0
    |hd::tl -> (hd* lst_lng tl) + lst2int tl;;

lst2int [2;3;4;5];;
lst2int [23;45];;
lst2int [23;4;5];;
  
