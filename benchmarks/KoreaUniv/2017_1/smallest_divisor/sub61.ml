(*#2*)
let smallest_divisor : int -> int = fun n ->
  if n < 0 then raise (Failure "n is negative number")
  else if n mod 2 = 0 then 2
  else let x = int_of_float (sqrt (float_of_int n)) in
    let rec f i n =
      if i > x then n
      else if n mod i = 0 then i
      else (f (i+2) n) in f 3 n;;