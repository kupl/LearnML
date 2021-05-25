let rec over_three n a =
  if a = 1 then true
  else
    let is_zero = n mod a in
    match is_zero with 
       0 -> false
      |_ -> over_three n (a-1) ;;
          
let prime : int -> bool
= fun n -> 
  if n = 1 then false
  else if n = 2 then true
  else let a = n - 1 in 
    over_three n a;;