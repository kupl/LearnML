let rec lst2int : int list -> int
= fun lst -> 
  let jun = List.rev lst in
  match jun with
    | [] -> 0
    | hd::tl -> hd + 10*lst2int (List.rev tl);;
    
lst2int [2;3;4;5];;

