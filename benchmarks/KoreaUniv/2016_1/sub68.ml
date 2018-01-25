(* Problem 1 *)
let rec fib a =
	if a = 0 then 0
	else if a = 1 then 1
	else fib (a -1) + fib (a - 2)

(* Problem 2 *)
let rec pascal (a, b) =
	if a = b then 1
	else if b = 0 then 1
	else pascal (a-1, b-1) + pascal (a-1, b)

(* Problem 3 *)
let rec pr_1 x y =
		if y = 1 then 1
		else if x mod y =  0 then 0
		else pr_1 x (y-1)

let prime a =
	if pr_1 a (a-1) = 1 then true
	else false

(* Problem 4 *)
let rec sigma (f : int -> int)  (x:int) (y:int) : int =
	if y - x = 0 then (f x)
	else (f x) + (sigma (f:int->int) (x+1:int) (y:int))
