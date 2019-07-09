let rec lst2int : int list -> int
= fun lst -> (*TODO*)
match lst with
  []->0
  |hd::tl->(lst2int tl)*10 +hd;;
  
lst2int[2;3;4;5];;