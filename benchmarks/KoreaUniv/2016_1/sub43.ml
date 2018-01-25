let rec fib n =	
	if n = 0 then 0 else
	if n = 1 then 1 else
	fib (n - 2) + fib ( n - 1 );;

let rec pascal (x , y)  =
	if x = 0 then 1 else
	if y = 0 then 1 else
	if x = y then 1 else
	(pascal ((x - 1),(y - 1))) + (pascal ((x - 1),(y)));;

let prime n = 
	let d = n - 1 in
	let rec is_divisor n d = 
		if d > 1 && n mod d = 0 then false else
		if d = 2 && n mod d <> 0 then true else
		is_divisor n (d-1) in
	 		if n > 2 then is_divisor n d else
	 		if n = 2 then true
			else false;;
 

let rec sigma test first second = 
	if first > second then 0 else
	test (first) + sigma test (first+1) second;;


