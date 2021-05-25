let prime : int -> bool
= fun n -> 
    let rec hasDivisor i =
      i * i > n || 
      (n mod i != 0 && hasDivisor (i+1)) in
    n != 1 && hasDivisor 2;;
    
    prime 2;;
    prime 3;;
    prime 4;;
    prime 7;;
    prime 14;;