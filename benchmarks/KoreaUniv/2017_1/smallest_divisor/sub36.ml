(* problem 2*)
  let rec div n x = if n mod x = 0 then x
  else div n (x+2)
  let smallest_divisor : int -> int
  = fun n -> if n = 0 then 0
  else if n = 1 then 1
  else if n mod 2 = 0 then 2
  else div n 3