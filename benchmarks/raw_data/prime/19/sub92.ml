exception Out_of_range;;

let prime : int -> bool = fun n -> 
  let rec check_prime n' a = 
    if a = 1 then true 
    else if n' mod a = 0 then false
         else check_prime n' (a-1)
  in match n with
    | 1 -> false
    | _ -> if n <= 0 then raise Out_of_range
           else check_prime n (n-1)
;;
