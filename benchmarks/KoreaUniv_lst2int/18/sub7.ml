let rec digit
= fun n times ->
  if n / 10 = 0 then times+1
  else digit (n / 10) (times+1);;

let rec sum
= fun l n -> match l with
  | hd::tl -> sum tl (n*(int_of_float (10.0**(float_of_int (digit hd 0)))) + hd)
  | [] -> n;;

let lst2int : int list -> int
= fun lst ->
  sum lst 0;;
