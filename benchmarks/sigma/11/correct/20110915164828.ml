
let rec sigma ((a:int), (b:int), (f:int -> int)) = 

if (a = b) then
f (a)
else 
f (a) + sigma (a+1, b, f)
;;


