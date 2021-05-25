let rec primecheck n a=
  if n=a then true
  else if n=1 then true
  else if (n mod a = 0) then false
  else primecheck n (a+1);;
  
let prime : int -> bool
= fun n -> primecheck n 2;;

prime 11;;