(* Problem 1 *)
let rec fib x =
if x = 1 then 1
    else if x = 0 then 0
    else fib(x-1)+fib(x-2)

(* Problem 2 *)
let rec pascal (x,y) =
  if y = 0 then 1
    else if y = x then 1
    else pascal(x-1,y-1)+pascal(x-1,y)

(* Problem 3 *)
let rec prime  = 
	let rec prime_iter x y =

        if y =1 then true
        else if y = 0 then false 
        else if x mod y = 0 then false
        else prime_iter x (y-1) in
    fun x -> prime_iter x (x-1)

(* Problem 4 *)
let rec sigma f a b =
 if a = b then f a 
    else f b + sigma f a (b-1)

