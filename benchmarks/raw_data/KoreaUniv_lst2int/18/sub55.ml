let lst2int : int list -> int
= fun lst ->
  let rec x lst num =
    match lst with
      | [] -> num
      | (hd::tl) -> x tl ((num*10) + hd)
      in x (lst) 0
    ;;


lst2int [2;3;4;5];;
lst2int [4;2;4;4];;