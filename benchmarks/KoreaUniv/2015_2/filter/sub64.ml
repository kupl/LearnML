(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter : ('a -> bool) -> 'a list -> 'a list 
=fun pred lst -> 
  if (List.length lst) > 0 then
    if (pred (List.hd lst)) then (List.hd lst)::(filter pred (List.tl lst))
    else filter pred (List.tl lst)
  else [] 
