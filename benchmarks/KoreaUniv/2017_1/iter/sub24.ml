
(* HW3 *)
  let rec iter n func i =
  if n > 1 then func (iter (n-1) func i)
  else func i
