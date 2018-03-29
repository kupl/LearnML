(*#8*)
let rec change : int list -> int -> int = fun coins amount ->
  if amount = 0 then 1
  else if amount < 0 then 0
  else match coins with
  |[] -> 0
  |hd::tl -> if hd < 1 then raise (Failure "a coin is smaller than 1")
             else(change tl amount) + (change coins (amount-hd));;
