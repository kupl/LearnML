(*problem 8*)
let rec change : int list-> int -> int = fun coins amount 
-> let rec total amount coins i
= if amount < 0 then 0
else if amount = 0 then 1
else if i = List.length coins then 0
else ((total (amount - (List.nth coins i)) coins i) + (total amount coins (i+1)))
in total amount coins 0;;
