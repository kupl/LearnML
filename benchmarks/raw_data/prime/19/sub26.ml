let prime : int -> bool
= fun n -> 
  let rec checkRemainderValues = fun (x,y) ->   (* returns false if a remainder value is 0 *)
    if y = 1 then true
    else if x mod y = 0 then false
    else checkRemainderValues (x, y - 1) in
  
  if n < 2 then false 
  else checkRemainderValues (n, n - 1);;