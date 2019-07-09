
let lst2int : int list -> int
= fun lst -> let rec sum = fun l1 ->
    match l1 with
      |[]->0
      |hd::tl -> hd+10*(sum tl) in
  sum (List.rev lst);;
  
lst2int [2;3;4;5];;