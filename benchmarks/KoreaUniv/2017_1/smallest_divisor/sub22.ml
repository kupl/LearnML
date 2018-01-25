(* problem 2*)

let smallest_divisor: int -> int
= fun n ->
let rec aux n d=
if(n mod 2 =0) then 2
else if float_of_int d> sqrt(float_of_int n) then n
else if (n mod d = 0) then d
else aux n (d+2) in aux n 3;;
