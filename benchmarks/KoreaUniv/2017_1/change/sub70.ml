(*problem 8*)
let change : int list -> int ->int
= fun coins amount -> 
let rec chang coins amount =
if amount = 0 then 1
else if amount < 0 then 0
else 
match coins with 
| [] -> 0
| hd::tl -> 
(chang tl amount) + (chang coins (amount-hd)) in chang coins amount;;
