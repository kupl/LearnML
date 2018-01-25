(* Problem 1 *)

let rec fib : int -> int
= fun num -> if num <= 0 then 0 else if num = 1 then 1 else fib(num-1) + fib(num-2);; 

(* Problem 2 *)

let rec pascal : int * int -> int
= fun (a, b) -> if a < 0 || b < 0 then 0 else if a = b then 1 else if b = 0 then 1 else 
				pascal (a -1, b -1) + pascal(a -1, b) ;; 

(* Problem 3 *)

let rec prime : int -> bool
= fun num -> if num<=1 then false else if num=2 then true else
			let rec div dd = if (dd*dd >num) then true else if (num mod dd= 0) then false else div(dd+1) in div 2;;


(* Problem 4 *)

let rec sigma : (int -> int) -> int -> int -> int
= fun f n1 n2 -> if n1<=n2 then f(n1) + sigma f(n1+1) n2 else 0;;