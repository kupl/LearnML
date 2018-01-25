(* problem 8*)
let rec length l = match l with |[]->0|hd::tl->1+length tl;;
let rec change : int list -> int -> int
= fun coins amount -> match coins with |[]->0|hd::tl-> 
if (length coins)= 1 then 
(if amount=0 then 0 else if amount mod hd = 0 then 1 else 0)
else if amount>0 then ((change tl amount)+(change coins (amount-hd)))
else if amount=0 then 1 else 0;;

