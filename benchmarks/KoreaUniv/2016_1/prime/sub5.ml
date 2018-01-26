let rec prime : int -> bool
= fun n ->
if n = 2 then true
else let rec division d = if d*d > n then true
  else (n mod d) <> 0 && division (d+1) in division 2

