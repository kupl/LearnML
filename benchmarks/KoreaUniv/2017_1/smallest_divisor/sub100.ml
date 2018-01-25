(* problem 2*)

let rec smallest_divisor2 n p =
  if (square p) > n then n
  else if n mod p = 0 then p
  else smallest_divisor2 n (p+1)

let smallest_divisor : int -> int
= fun n -> smallest_divisor2 n 2