(* problem 8*)

let change : int list -> int -> int
= fun coins amount -> 
 if amount = 0 then 1
 else if ((amount < 0) || (coins = [])) then 0
 else 0

