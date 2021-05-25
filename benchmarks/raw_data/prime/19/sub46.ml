let rec prime : int -> bool
= fun n -> 
  let rec checkRemainder x d = match d with
    | 1 -> true    
    | _ -> (x mod d <> 0) && checkRemainder x (d-1)
    in match n with
      | 0 | 1 -> false
      | _ -> checkRemainder n (n-1) ;;
      

prime 4;;
prime 7;;