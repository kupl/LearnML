exception NO_NEGATIVE_INTEGERS
(* problem 2*)

let smallest_divisor : int -> int
= fun n -> 
if n<0 then raise NO_NEGATIVE_INTEGERS
else if n mod 2 == 0 then 2
else
  let rec loop n d =
    if (d*d)>n then n
    else
      if n mod d == 0 then d
      else loop n (d+2) in
        loop n 3