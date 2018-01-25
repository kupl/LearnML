  (*Problem 5*)
(*1. not using "product" of Problem 4*)
let rec dfact n = if (n=0)||(n=1) then 1 else n*dfact(n-2);;
  
(*2. using similar "product" of Problem 4*)
let rec product2 f a b =
if a<b then (f b)*(product2 f a (b-2))
else if a>b then (f a)*(product2 f (a-2) b)
else f a;;

let dfact n =
if (n mod 2)=0 then product2 (fun x -> x) 2 n
else product2 (fun x -> x) 1 n;;