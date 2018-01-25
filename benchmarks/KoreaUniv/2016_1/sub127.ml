(* Problem 1 *)
let rec fib : int -> int
= fun n ->if n>2 then fib(n-1) + fib(n-2)
					else 1;;

(* Problem 2 *)
let rec fac n =
	if n>1 then n*fac(n-1)
	else 1;;
let rec pascal : int * int -> int
= fun (n1, n2) -> if n1>=n2 then fac(n1)/fac(n2)/fac(n1-n2)
							    else 1;;

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> let n = abs n in
					   let rec divisor d = 
							 d*d>n || (n mod d <> 0 && divisor(d+1)) in
							 n<>1 && divisor 2;;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a<=b then f(a) + sigma f (a+1) b
							 else 0;;



let func n
= if(n=0) then 1 else 2
let result = func 1


let rec func n
= if(n=1) then 1 else (n-1)*func(n)
let result = func 2

let rec f n =
  if (n  = 1) then 1
  else if (n = 0) then 0
  else (f (n-1)) + (f (n-2));;

let result = f 0;;

{
	0 -> 1;
}

