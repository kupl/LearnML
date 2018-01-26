let rec prime n =
 let n = abs n in
 let rec divisor d =
 d*d > n || (n mod d <> 0 && divisor (d+1)) in
 n <> 1 && divisor 2;;
