(* problem 2*)
  let smallest_divisor : int -> int
  = fun n -> let d = 2 in
  let rec divisor : int -> int -> int
  = fun d n -> if (int_of_float (sqrt (float_of_int n))) >= d then
  if (n mod d) = 0 then d else divisor (d+1) n else n in
  divisor d n;;