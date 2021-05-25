let prime : int -> bool
= fun n -> (*TODO*)
if n=1 then true
else let rec checkModulo x y= 
  if x mod y !=0 && y>1 then checkModulo x (y-1) 
  else if x mod y =0 && y>1 then false 
  else true in checkModulo n (n-1) ;;