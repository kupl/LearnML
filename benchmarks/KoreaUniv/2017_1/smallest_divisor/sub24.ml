
(* HW2 *)
  let rec  modi n b =
  if b*b>n then n
  else
  if n mod b = 0 then b else modi n (b+1)

  let  smallest_divisor n = 
    modi n 2
