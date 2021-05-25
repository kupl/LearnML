let prime : int -> bool
= fun n ->
  if n = 1 then false
  else
    let rec iden x y =
      if y * y > x then true
      else if x mod y = 0 then false
      else iden x (y + 1)
    in iden n 2;;