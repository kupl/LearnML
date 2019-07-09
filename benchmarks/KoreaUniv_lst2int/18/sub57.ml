let lst2int : int list -> int
= fun lst -> 
  let rec inner l = 
    match l with 
      | [] -> ""
      | hd::tl -> (string_of_int hd) ^ (inner tl)
  in int_of_string(inner lst);;
    
lst2int [2;3;4;5;6;7;8;8];;
