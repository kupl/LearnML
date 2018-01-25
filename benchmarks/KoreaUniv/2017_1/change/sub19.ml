  (* problem 8*)

  let rec change : int list -> int -> int
  = fun coins amount -> let zero = 0 in if amount = 0 then 1
  else if amount < zero then 0
  else match coins with
  |[] -> 0
  |hd::tl -> change coins (amount-hd) + change tl amount;;

