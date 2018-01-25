let square x = x*x;;

let rec fastexpt b n=
if(n=0) 
  then 1
  else
  if(n mod 2=0)
  then square(fastexpt b (n/2))
  else b*(fastexpt b (n-1));;

let rec product: (int->int)->int->int->int
=fun f a b ->if(a=b) then f a
else (f (a))*(product f (a+1) b);; 

let rec dfact:int->int
=fun n->if(n mod 2=0) then (product (fun x->x) 1 (n/2))*(fastexpt 2 (n/2))
  else ((product (fun x->x) 1 n)/(dfact (n-1)));;