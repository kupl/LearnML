(* problem 2*)

let rec div_helper : int -> int -> int -> int
= fun n div limit ->
  if (n mod div) = 0 then div
  else if div >= limit then n
       else div_helper n (div+2) limit

let smallest_divisor : int -> int
= fun n ->
  if (n mod 2) = 0 then 2
  else let limit = (int_of_float (sqrt (float_of_int n))) in
       let result = (div_helper n 3 limit) in
       result