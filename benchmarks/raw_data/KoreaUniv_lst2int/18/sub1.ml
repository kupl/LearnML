let rec lst2int : int list -> int
= fun lst ->
  match lst with
    [] -> int_of_string ""
    | hd::tl ->
      int_of_string (string_of_int (hd) ^ string_of_int (lst2int tl));;

lst2int [2;3;4;5];;