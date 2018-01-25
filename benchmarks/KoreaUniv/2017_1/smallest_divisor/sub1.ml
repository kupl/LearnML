let smallest_divisor : int -> int
 = fun n ->let rec smd y n = if n mod y = 0 then y else if y*y > n then n else smd (y+1) n in smd 2 n