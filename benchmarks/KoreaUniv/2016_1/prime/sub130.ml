let rec divisible : int -> int -> bool
  = fun n d ->  
  if d * d <= n && n mod d != 0 then divisible n (d + 1)
  else if n mod d != 0 then true
  else false;;

let d = 2

let prime : int -> bool
= fun n -> 
	if n == 2 then true
	else if divisible n d == true then true
	else false
