let lst2int : int list -> int
= fun lst -> 
  let rec f l result
  = match l with
    | hd::tl -> f tl (result * 10 + hd)
    | [] -> result
  in f lst 0;;
