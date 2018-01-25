
(* HW4 *)
  let rec product f a b = 
  if a=b then a
  else f a (product f (a+1) b)

