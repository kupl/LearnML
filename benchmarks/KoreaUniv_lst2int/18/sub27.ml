let lst2int : int list -> int
= fun lst -> let rec cal : int list -> int -> int
= fun lst ans -> match lst with
  | [] -> ans
  | h::t -> cal t (ans*10+h)
  in cal lst 0;;
  
lst2int [2;3;4;5];;