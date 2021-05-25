let rec subprime n m =
  if n = m then true else
  if n mod m = 0 then false
  else subprime n (m+1);;

let prime : int -> bool
= fun n -> subprime n 2;;