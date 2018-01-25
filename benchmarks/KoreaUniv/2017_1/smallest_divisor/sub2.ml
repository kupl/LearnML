(*2*)
  let smallest_divisor : int -> int
  =fun n ->
  let rec divisor n m=
  if m>(n/2) then n
  else if (n mod m=0) then m
  else divisor n (m+1) in divisor n 2;;