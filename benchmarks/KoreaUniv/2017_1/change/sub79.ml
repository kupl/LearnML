let rec change : int list -> int -> int
= fun coins amount -> 
  if amount = 0 then 1
  else if amount < 0 then 0
  else match coins with
  | [] -> 0
  | h::t -> (change coins (amount - h)) + (change t amount)