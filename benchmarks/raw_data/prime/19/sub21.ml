let rec prime : int -> bool
= fun n ->
  
  let square x = x*x in 
  
  let rec finding : int -> bool 
  = fun x ->
      match n with
        | 1 -> false
        | _ ->
          if (square x) > n then true
          else if (n mod x) = 0 then false
          else finding (x+1)
  
  in (finding 2);;

(*output*)
let result_false = prime 33;;
let result_true = prime 29;;
prime 4;;
prime 1;;