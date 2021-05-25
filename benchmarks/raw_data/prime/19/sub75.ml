let prime : int -> bool
= fun n -> (*TODO*)
  let rec inprime n a =
    if a = 1 then true else
    if (n mod a) = 0 then false else inprime n (a - 1)
    
    in if n < 2 then false else inprime n (n-1);;
    
    
prime 2;;
prime 3;;
prime 4;;
prime 17;;