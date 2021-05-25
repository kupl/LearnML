let rec lst2int : int list -> int
= fun lst -> let rec change lis num =
  match lis with
    hd::tl -> change tl (hd + 10*num)
    | [] -> num
    in change lst 0;;

lst2int [2;3;4;5];;