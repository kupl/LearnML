(* problem 2*)
let smallest_divisor : int -> int
= fun n->
  let rec calc n i=
    if i>int_of_float(sqrt(float_of_int(n))) then n
    else if n mod 2=0 then 2
    else if n mod i=0 then i else calc n (i+2) in
  calc n 3;;