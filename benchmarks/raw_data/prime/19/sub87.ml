let prime : int -> bool
= fun n -> (*TODO*)
    match n with 
    | 0 | 1 -> false
    | _ -> let rec help n m = 
      match m with 
      | 1 -> true
      | _ -> (n mod m <> 0) && help n (m-1) 
       in help n (n-1);;
       
prime 2;;
prime 3;;
prime 4;;
prime 17;;