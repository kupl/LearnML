(*Probelm 1*)

let rec fib x = if x=1 then 0
								else if x=2 then 1
								else fib (x-1) + fib (x-2);;


(*Problem 2*)

let rec pascal (x,y) = if y=0 then 1
											 else if x=y then 1
											 else pascal (x-1,y-1) + pascal (x-1,y);;


(*Probelm 3*)

let countz z = if z=2 then true else false;;
let rec divide x y z = if x<y then countz z
											 else if x mod y = 0 then divide x (y+1) (z+1)
											 else divide x (y+1) z;;
let prime x = divide x 1 0;;


(*Problem 4*)

let is_enough x y = if x>y then true else false;;
let sum f x y = let x = (f) x in x+y;;
let rec sigmaloop f x y z = if is_enough x y then z else sigmaloop f (x+1) y (sum f x z);;
let sigma f x y = sigmaloop f x y 0;;
