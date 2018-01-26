let rec checkPrime n x 
= if x * x > n then true
  else if n mod x = 0 then false
  else checkPrime n (x+1)

let rec prime : int -> bool
= fun n -> checkPrime n 2
