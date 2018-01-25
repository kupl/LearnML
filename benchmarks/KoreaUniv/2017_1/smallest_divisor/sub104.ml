(* problem 2*)

let smaller_root a n = if (n*n)<a then true else if (n*n)=a then true else false
let check_div a n = if (a/n)*n=a then true else false

let rec divisor a n = if smaller_root a n then (if check_div a n then n else divisor a (n+1)) else a
  
let smallest_divisor : int -> int
= fun n -> divisor n 2