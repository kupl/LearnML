let rec compare n a =
  if n >= a * a then compare n (a+1) else (a-1);;

let sqrt n = compare n 2;;

let rec isprime n a =
    match a with
      1 -> true
      | _ -> if n mod a <> 0 then isprime n (a-1) else false;;
  
let prime : int -> bool
= fun n -> if n = 1 then false else isprime n (sqrt(n));; (*TODO*)
            
prime 2;;
prime 3;;
prime 4;;
prime 17;;