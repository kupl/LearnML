(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> if List.length coins > 0 && amount = 0 then 1
else if amount < 0 || (List.length coins = 0 && amount > 0) then 0
else match coins with 
| [] -> 0
| hd :: tl -> (change tl amount) + (change coins (amount - hd));;



