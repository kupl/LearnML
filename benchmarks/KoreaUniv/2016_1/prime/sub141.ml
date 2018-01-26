let rec prime : int -> bool
= fun n -> (* TODO *)
let rec not_divided d =
(d*d > n) ||( ( (n mod d) <> 0 ) && not_divided (d+1) ) in
(n > 1) && (not_divided 2)
