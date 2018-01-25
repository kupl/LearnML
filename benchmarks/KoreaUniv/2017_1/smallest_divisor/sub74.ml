(*Problem 2*)
let checksqrt a n = a > sqrt(n)

let rec find_sd a n = if (checksqrt (float_of_int a) (float_of_int n)) then n
  else (if (n mod a = 0) then a else find_sd (a+2) n)

let smallest_divisor : int -> int
= fun n -> if (n mod 2 = 0) then 2 else
  (
    let a = 3 in
    find_sd a n
  )