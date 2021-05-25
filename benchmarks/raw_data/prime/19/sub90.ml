let rec iter_divisible dividend divisor = 
  if divisor * divisor > dividend then true
  else if divisor > 1 && dividend mod divisor = 0 then false
  else iter_divisible dividend (divisor + 1);;

let prime : int -> bool
= fun n -> 
  if n < 2 then false
  else iter_divisible n 1;;
