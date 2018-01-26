let rec prime_inner : int -> int -> bool
= fun n d ->
	if n < 1 then false
	else if n <= d then true
	else if n mod d = 0 then false
else (prime_inner n (d+1));;

let rec prime : int -> bool
= fun n -> 
	prime_inner n 2;;
