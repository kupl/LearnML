let rec pri : int -> int -> bool
= fun count n ->
  if count * count > n then true
  else if n mod count = 0 then false
  else pri (count + 1) n;;

(* Problem 3 *)
let rec prime : int -> bool
= fun n ->
  if n = 1 then false
  else pri 2 n;;
