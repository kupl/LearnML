let rec redivide n divider =
  if divider = 1 then 0
  else if n mod divider = 0 then -1
  else redivide n (divider-1)
;;

let prime : int -> bool
  = fun n -> 
    if redivide n (n-1) = 0 then true
    else false
;;
