let rec prime n = let rec primein (num, i) =
if i = 1 then 1
else if i = 0 then 0
else 
if (num mod i) = 0 then 0 else primein (n, i-1)
in if primein (n, n/2) = 1 then true
else if n < 0 then raise (Failure "Error")
else false;;  
