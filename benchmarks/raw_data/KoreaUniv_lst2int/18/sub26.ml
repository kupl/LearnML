let lst2int : int list -> int
= fun lst -> 
  let rec lst2int' lst num =
        match lst with 
        | [] -> num
        | hd::tl -> lst2int' tl ((num * 10 ) + hd)
  in lst2int' lst 0;;
  
  lst2int [2;3;4;5];;
