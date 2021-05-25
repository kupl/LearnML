let rec divi n k
= if k < n then
if n mod k = 0 then false else divi n (k+1)
else if n = 1 then false
else true;;

let prime n = divi n 2;;
