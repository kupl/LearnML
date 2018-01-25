let product : (int -> int) -> int -> int -> int
= fun f a b -> 
  if a > b then 1
  else if a = b then f a
  else 
    let rec innerProd 
    = fun f n b c ->
      if n > b then c
      else if n = b then c * f n
      else innerProd f (n + 1) b (c * f n) in 
      
      innerProd f a b 1;;

let dfact : int -> int
= fun n ->
  let innerK = fun x ->
    if n mod 2 = 0 then 2 * x
    else 2 * x - 1 in
  if n mod 2 = 0 then product innerK 1 (n / 2)
  else product innerK 1 ((n + 1) / 2);;