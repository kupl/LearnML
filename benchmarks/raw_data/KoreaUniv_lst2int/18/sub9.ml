let lst2int : int list -> int
= fun lst -> 
  let rec calc_int : int list -> int -> int
  = fun l num -> match l with
    | [] -> num
    | hd::tl -> (calc_int tl (num * 10 + hd))
    
  in
    calc_int lst 0;;