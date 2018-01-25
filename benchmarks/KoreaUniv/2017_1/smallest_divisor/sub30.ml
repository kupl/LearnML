(* problem 2*)
let abs n = if n > 0 then n else (-n)
let isdivisor n test = (n mod test) = 0
let rec findodd n odd = 
	if (odd*odd) > n then n
	else if isdivisor n odd then odd
	else findodd n (odd+2)

let smallest_divisor : int -> int
= fun n -> 
	if n=0 || n = 1 then n
	else if isdivisor n 2 then 2
	else findodd (abs n) 3