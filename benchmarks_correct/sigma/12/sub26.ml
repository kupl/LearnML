
let rec sigma f a b = 
  if a > b then 0
  else if a = b then (f a)
  else (f a) + sigma f (a+1) b

  (*
let f x = x*x

let a = 1
let b = 10

let _ = print_int (sigma (a,b,f))
*)
