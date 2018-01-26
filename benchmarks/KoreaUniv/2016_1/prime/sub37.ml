let rec prime : int -> bool
= fun n -> let rec divide d = (d = n) || (n mod d <> 0 && divide(d+1)) in n>1 && divide 2;;  (* TODO *)