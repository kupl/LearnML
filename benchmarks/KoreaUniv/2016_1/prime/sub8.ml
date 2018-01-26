let rec prime : int -> bool
= fun n ->
let n = abs n in 
let rec notDivisor d =
d * d > n || (n mod d <> 0 && notDivisor (d+1)) in
n <> 1 && notDivisor 2;;
