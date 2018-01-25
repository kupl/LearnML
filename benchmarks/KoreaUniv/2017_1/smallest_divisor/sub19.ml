  (* problem 2*)

  let smallest_divisor : int -> int
  = fun n -> let rec num n i = if n = 1 then 1
  else if (n mod i = 0 ) then i
  else num n (i+1) in num n 2;;