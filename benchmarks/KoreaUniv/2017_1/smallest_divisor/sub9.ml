(* problem 2*)

let smallest_divisor : int -> int
= fun n ->
let rec div : int -> int -> int
= fun n cnt ->
if (n mod cnt) = 0 then cnt else div n (cnt+1) in div n 2