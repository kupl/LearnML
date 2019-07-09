let lst2int : int list -> int
= fun lst ->
  let rec l2i _list _int =
    match _list with
      |[] -> _int
      |hd::tl -> l2i tl (_int*10 + hd)
  in l2i lst 0;;
