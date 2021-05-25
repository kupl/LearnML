let prime : int -> bool
= fun n -> let rec chk n a =
  if (a*a)>n then true else 
    (if n mod a = 0 then false else chk n (a+1))
    in chk n 2;;
    
prime 2;;
prime 3;;
prime 4;;
prime 9;;
prime 17;;
prime 35;;