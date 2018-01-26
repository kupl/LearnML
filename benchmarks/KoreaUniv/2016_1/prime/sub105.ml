let rec prime:int->bool
=fun n-> let a= n-1 in let rec divisor n a= 
if a>1 && n mod a=0 then false else if a=2 && n mod a<>0 then true
else divisor n (a-1) in 
if n>2 then divisor n a
else if n=2 then true else false;;
