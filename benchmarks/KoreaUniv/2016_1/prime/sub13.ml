let rec prime : int -> bool 
= fun n -> let rec is_divided d
= d*2 > n || (n mod d <> 0 && is_divided (d+1))
in n <> 1 && is_divided 2;;
     