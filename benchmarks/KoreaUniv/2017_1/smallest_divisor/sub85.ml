(* problem 2*)

let smallest_divisor : int -> int
= fun n ->
 let rec find_divisor n d =
if (n mod d) = 0 then d
else if (fun x -> x*x) d > n then n
else find_divisor n (d+1)
in find_divisor n 2;;