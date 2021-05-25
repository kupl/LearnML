
let prime : int -> bool
= fun n -> 
    match n with
      | 1 -> true
      | 2 -> true
      | _ ->  let a = 2 in
              let rec isPrime n a =
               if a = n then true else
                 match  n mod a with
                   | 0 -> false
                   | _ -> isPrime n (a+1)
             in isPrime n a
                
                
                
;;

prime 2 ;;
prime 3 ;;
prime 4 ;;
prime 17 ;;

