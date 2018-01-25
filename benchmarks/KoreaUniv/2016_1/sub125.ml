let rec fib n = match n with
	0 -> 0
	|1 -> 1
	|_ -> fib(n-1)+fib(n-2);;

let rec pascal (x , y)  =
  if snd (x , y) == 0 then 1
	else if snd (x , y) == fst (x , y) then 1 
	else pascal (x - 1 , y - 1) + pascal (x - 1 , y);;

let rec prime n =
	let n = abs n in
	let rec is_not_divisor d =
		d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
	n <> 1 && is_not_divisor 2;;

let rec sigma (f : int -> int) a b = if a <= b then f a + sigma f (a+1) b
else 0;;
    