(*#5*)
let rec dfact : int -> int = fun n ->
  if n < 0 then raise (Failure "n is negative number")
  else if n = 1 || n = 0 then 1
  else if n = 2 then 2
  else n * (dfact (n-2));;