(* problem 2*)

let smallest_divisor : int -> int
= fun n ->
  let rec cnt_fun n k =
  if n mod k = 0 then k
  else if int_of_float (sqrt (float_of_int n)) = k then n
  else cnt_fun n (k+1)
    in cnt_fun n 2