(* Problem 1 *)
let rec fib : int -> int
= fun n -> if(n<=1) then n
					 else fib(n-1) + fib (n-2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if(n1<0||n2<0) then -1
									else if(n1=n2||n2=0) then 1
									else pascal(n1-1,n2-1) + pascal(n1-1,n2) 

(* Problem 3 *)
let rec check = fun n x -> if(n<=1) then false
								else if(x>n/2) then true
								else if((n mod x)=0) then false
								else check n (x+1)

let prime (*재귀 생각을 못해 일반 함수로 바꾸어 풀었습니다.*)
= fun n -> check n 2


(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if(a>b) then 0
							 else sigma f a (b-1) + f(b)

