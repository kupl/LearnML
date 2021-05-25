let rec checkPrime a b =
  if (b >= a) then true else ((a mod (b) <> 0) && checkPrime a (b+1));;

let prime : int -> bool
= fun n -> 
  match n with
    | 0 -> false
    | 1 -> false
    | _ -> checkPrime n 2;;

prime 864;;