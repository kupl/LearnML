let prime n =
  let rec checkZero x d = match d with
    | 1 -> true
    | _ -> (x mod d <> 0) && checkZero x (d-1)
    in match n with
      | 0 | 1 -> false
      | _ -> checkZero n (n-1);;
      
prime 2;;
prime 3;;
prime 4;;
prime 5;;
prime 6;;
prime 7;;