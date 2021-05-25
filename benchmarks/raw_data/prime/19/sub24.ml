let a = 2;;

let rec rest b a =
  if a > (b/2) then 0
  else if b mod a = 0 then a
  else rest b (a+1);;

let prime : int -> bool
= fun n ->
  if n < 2 then raise (Failure "n is smaller than 2")
  else if rest n a > 0 then false
  else true;;
  
prime 47;;