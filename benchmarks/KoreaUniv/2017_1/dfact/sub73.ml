exception NO_NEGATIVE_INTEGERS
(* problem 5*)

let dfact : int -> int
= fun n -> 
if n<0 then raise (NO_NEGATIVE_INTEGERS)
else if n == 0 then raise ANSWER_TO_BE_1
else
  if n mod 2 == 0 then
    let rec even n d =
      if n == d then n
      else d * even n (d+2) in even n 2
  else
    let rec odd n k =
      if n == k then n
      else k * odd n (k+2) in odd n 1
