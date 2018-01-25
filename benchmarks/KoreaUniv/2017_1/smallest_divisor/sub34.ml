 let rec smallest_divisor : int -> int
= fun n ->
 
let rec get_divisor : int -> int -> int =
fun n d ->
if n mod d = 0 then d
else if d * d < n then get_divisor n (d+2)
else n
 
in
 
if n mod 2 = 0 then 2
else get_divisor n 3