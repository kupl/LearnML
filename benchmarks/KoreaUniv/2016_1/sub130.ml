let rec fib : int -> int
= fun n ->  
 	if n ==0 then 0
	else if n == 1 then 1
	else fib (n - 2) + fib (n - 1)


let rec pascal : int * int -> int
= fun (n1, n2) ->
	if n1 < 0 || n2 < 0 || n1 < n2 then -1
	else if n1 == 0 || n2 == 0 || n1 == n2 then 1
	else pascal (n1 - 1, n2 - 1) + pascal (n1 - 1, n2)

let rec divisible : int -> int -> bool
  = fun n d ->  
  if d * d <= n && n mod d != 0 then divisible n (d + 1)
  else if n mod d != 0 then true
  else false;;

let d = 2

let prime : int -> bool
= fun n -> 
	if n == 2 then true
	else if divisible n d == true then true
	else false



let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 
	if a == b then f a
	else (f a) + (sigma f (a+1) b)
