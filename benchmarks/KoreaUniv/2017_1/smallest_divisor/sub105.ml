(* problem 2*)
let rec smallest_divisor_finder : int -> int -> int
= fun n d ->
  if n mod d = 0 then d
  else if d * d > n then n
  else smallest_divisor_finder n (d+1);;

let smallest_divisor : int -> int
= fun n ->
  if n mod 2 = 0 then 2
  else smallest_divisor_finder n 3;;