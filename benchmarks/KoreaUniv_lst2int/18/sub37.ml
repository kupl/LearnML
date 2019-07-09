let rec func lst =
  match lst with 
    |[]-> 1
    |hd::tl -> 10 *(func tl );;
    
let rec lst2int : int list -> int
= fun lst -> 
    match lst with
      | [] -> 0  
      | hd :: tl -> hd*(func tl) + (lst2int tl);;

lst2int [2;3;4;5];;
