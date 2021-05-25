let rec p : int -> int -> bool
= fun n x -> if x * x > n then true
  else if n mod x = 0 then false
  else p n (x+1);;

let prime : int -> bool
= fun n -> if n = 1 then false else let x = 2 in p n x;;
    
prime 2;;
prime 3;;
prime 4;;
prime 17;;