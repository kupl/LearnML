let prime : int -> bool
= fun n -> 
  let rec modular_N_to_1 a n =
    if a = 1
    then true
    else
      if n mod a = 0
      then false
      else modular_N_to_1 (a - 1) n
  in 
    if n >= 2
    then modular_N_to_1 (n - 1) n
    else false;;