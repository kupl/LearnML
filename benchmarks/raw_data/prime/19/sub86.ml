let prime : int -> bool
= fun n -> (*TODO*)
  let n = n in
  let rec is_divided_by d =
    if (d*d > n) then true
    else if (n mod d <> 0) && (is_divided_by (d+1)) then true
    else false in
        if (n = 1) || (n=0) then false
        else is_divided_by 2;;
      