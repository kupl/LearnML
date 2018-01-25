(* problem 1*)
  let rec f b n = 
  if n = 0 then 1 
  else b * (f b ((n-1)/2)) * (f b (n/2))