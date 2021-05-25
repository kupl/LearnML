let rec myprime n x =
  if x = 1 then true
  else if n mod x = 0 then false else myprime n (x-1);;


let prime : int -> bool
= fun n ->
  if n = 1 then false
  else if n = 2 then true
  else
    let x = n - 1 in
      myprime n x;;

prime 2;;
prime 3;;
prime 4;;
prime 17;;