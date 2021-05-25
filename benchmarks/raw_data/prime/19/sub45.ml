
let rec cal a b= 
  if b = 1 then true
  else if a mod b = 0 then false
  else cal a (b-1);;
  
let prime n = cal n (n-1);;

(*함수가 잘 작동하는지 시험*)
prime 17;;
  

