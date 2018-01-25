(* problem 2*)

let smallest_divisor : int -> int
= fun n -> let m = 2 in
  let rec func n m =
  if m * m > n then n
  else (if n mod m = 0 then m
  else func n (m+1)) in func n m;;