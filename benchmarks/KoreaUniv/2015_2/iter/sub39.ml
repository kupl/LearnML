let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) num-> 
if n = 0 then num
else f (iter (n-1,f) num);;
