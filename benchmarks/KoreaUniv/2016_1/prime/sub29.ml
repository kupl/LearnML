let rec prime : int -> bool
= fun n -> if n = 1 then false
 else md n 2 and
 md n k = if n=k then true
 else if n mod k=0 then false
 else md n (k+1)
