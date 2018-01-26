let rec prime : int -> bool
= fun n ->
if n <= 1 then false
else let rec not_divisor d = (d*d > n) || (n mod d <> 0 && not_divisor (d+1)) in (not_divisor 2)
