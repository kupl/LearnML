let rec fib : int -> int
	= fun n -> if n=0 then 0
		else if n=1 then 1
		else fib (n-2) + fib (n-1);;

let rec pascal : int*int -> int
	=fun(a,b)->
		if b=0 || a = b then 1
		else pascal(a-1,b-1) + pascal (a-1,b);;
 
let rec sigma : (int -> int) -> int -> int -> int
	= fun f a b -> if (a=b) then f(b) else f(a)+sigma f (a+1) (b);;
 
let rec prime : int -> bool
	= fun n -> if n<2 then false
		else if n=2 then true
		else let rec div d =
			if (n mod d = 0 && n > d) then false
			else if (n=d) then true
			else div (d+1) in div 2;;

