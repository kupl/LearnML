let rec prime_ n a =
  if n = 2 || n=a then true
  else if n mod a = 0 then false
  else prime_ n (a+1);;
  
  
  
let prime : int -> bool = fun n -> prime_ n 2;;
