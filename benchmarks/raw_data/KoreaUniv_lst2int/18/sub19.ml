
let rec lst2int : int list -> int
= fun lst -> match lst with
  | []-> 0
  | hd::[]-> hd
  | hd::tl ->hd*int_of_float(10.**float_of_int (List.length tl))+lst2int tl;; 
  

  
lst2int [1;2;3;5];;
