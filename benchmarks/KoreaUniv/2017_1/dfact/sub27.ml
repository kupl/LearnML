(*problem 4*) let rec product: (int->int) ->int->int->int=fun f a b -> 
if a= b then f a
else (f b) *  product f a (b-1)

(* problem 5*) let dfact: int ->int =fun n->
if n=0 then 1
  else  if n mod 2= 0 then product (fun x ->2*x) 1 (n/2)
else product (fun x-> 2*x-1) 1 ((n+1)/2)