let rec max : int list -> int
=fun l ->   
  let rec compare : int -> int -> int 
  =fun a b ->
    if a > b then a
    else b
  in if (List.length l) > 1 then
    if (List.hd l) > (List.hd (List.tl l)) then compare (List.hd l) (max (List.tl l))   
    else max (List.tl l)
  else List.hd l
 