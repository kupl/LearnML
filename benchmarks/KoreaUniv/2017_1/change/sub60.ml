(* problem 8*)

let rec change : int list -> int -> int 
= fun coins amount -> (*TODO*)
if (amount=0) then 1
else if amount<0 then 0 
else match coins with 
| hd::tl -> (change coins (amount-hd))+(change tl amount)
| [] -> 0 
