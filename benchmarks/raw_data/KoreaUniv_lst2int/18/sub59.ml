let rec lst2int : int list -> int
= fun lst -> 
  match (List.rev lst) with
    |[]-> 0
    |hd::tl -> hd+10*(lst2int (List.rev tl));;  
    
lst2int [2;3;4;5;6;7;8;9];;

