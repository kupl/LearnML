let lst2int : int list -> int
= fun lst -> (*TODO*)
  let rec lst2int2 lst x = 
    match lst with
      | [] -> x
      | h::t -> lst2int2 t (x*10+h)
    in lst2int2 lst 0;;

lst2int [2;3;4;5];;