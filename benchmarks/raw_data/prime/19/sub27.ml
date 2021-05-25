let prime : int -> bool
= fun n ->  let n = n in
    let rec divisor d =
      d * 2 > n || (n mod d <> 0 && divisor (d+1)) in
                                                   n <> 1 && divisor 2;;
    
    
    prime 7;;
    prime 8;;
    prime 13;;