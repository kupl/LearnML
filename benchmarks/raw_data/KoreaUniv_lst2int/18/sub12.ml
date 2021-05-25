let lst2int : int list -> int
= fun lst ->
  let rec makeInt intlst n =
    match intlst with
      | [] -> n
      | hd::tl -> makeInt tl (10*n+hd) in makeInt lst 0;;
      
      
lst2int [2;3;4;5];;
