let rec prime_help : int -> int -> bool
= fun d n ->
  if d * d > n then true
  else if (n mod d) = 0 then false
  else prime_help (d + 1) n
    
let prime : int -> bool
= fun n -> 
  if n < 1 then raise (Failure "Invalid Input")
  else if n = 1 then false
  else prime_help 2 n
;;