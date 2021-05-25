let rec is_prime: int * int -> bool
= fun (n, r) -> match r with
    | 1 -> true
    | _ -> if n mod r = 0 then false else is_prime (n, r-1);;
    
let prime : int -> bool
= fun n -> match n with
    | 1 -> false
    | _ -> if n < 0 then false 
           else is_prime (n, n-1);;
    

  
