(* hw1 ex2 "sigma" *)


let rec sigma (a,b,f) : int = 
  if a > b then 0
  else if a = b then (f a)
  else (f a) + sigma (a+1,b,f)
