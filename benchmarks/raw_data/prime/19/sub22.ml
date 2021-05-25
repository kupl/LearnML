let rec prime_sub n a = 
  if n<2 then false
  else if a = n then true
  else if n mod a = 0 then false
  else prime_sub n (a+1);;

let prime : int -> bool
= fun n -> (*TODO*)
  prime_sub n 2;;
  
