let rec isprime n a =
  if n mod a = 0 then false
  else if n < a*a then true
  else isprime n (a + 1);;

let prime : int -> bool
= fun n -> if n = 2 then true else isprime n 2;;

prime 173;;