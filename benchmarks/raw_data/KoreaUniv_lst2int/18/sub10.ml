let rec reverse_lst lst =
  match lst with
    [] -> []
    |hd::tl -> reverse_lst tl @ [hd];;

let rec my_lst2int lst =
  match lst with
    []->0
    |hd::tl -> hd + 10*(my_lst2int tl);;

let lst2int : int list -> int
= fun lst -> my_lst2int (reverse_lst lst);;

lst2int [2;3;4;5] = 2345;;