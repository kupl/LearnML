let rec diva n a =
  if n <= a then true
  else if n mod a = 0 then false
  else diva n (a+1);;

let prime : int -> bool
= fun n -> if n < 2 then false
          else diva n 2;;(*TODO*)
