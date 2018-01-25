(*problem 2*)
let smallest_divisor : int -> int
= fun n ->
let rec findfactor n factor = 
if factor * factor > n then n
else if n mod factor = 0 then factor
else findfactor n (factor+1) in findfactor n 2;;