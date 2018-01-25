exception Problem;;

(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
  if n=0 then 1
  else if n mod 2 = 0 then (fastexpt b (n/2))*(fastexpt b (n/2))
  else b*(fastexpt b (n-1));;


(* problem 2*)

let rec oddmod n odd nsqrt =
	if odd<=nsqrt then (if n mod odd = 0 then odd
								else (oddmod n (odd+2) nsqrt))
	else n;;

let rec smallest_divisor : int -> int
= fun n ->
	let nsqrt = int_of_float(sqrt(float_of_int(n))) in
	if n mod 2 = 0 then 2(*짝수일때*)
	else oddmod n 3 nsqrt;;


	(*홀수일때 sqrt해주고 3부터 시작하는 홀수이자 소수로 나눠지는 것 찾기 안나눠지면 입력된 n이 smallest_divisor*)




(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->  
	if n=0 then fun x->x
	else if n>0 then fun x->f(iter(n-1, f) x)
	else raise Problem;;



(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> 
	if a=b then f(a)
	else if a<b then (product f a (b-1))*(f b)
	else raise Problem;;

(* problem 5*)

let dfact : int -> int
= fun n ->
	if n mod 2 = 0 then (product (fun x->2*x) 1 (n/2))
	else (product (fun x->(2*x-1)) 1 ((n+1)/2));;



(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n ->
	if n<=0 then l
	else (
		match l with
		|[]->[]
		|hd::tl->drop tl (n-1)
		);;


(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
	match lst with
	|[]->([],[])
	|hd::tl-> match hd with 
			|(a,b) -> let l1, l2 = unzip tl in a::l1, b::l2;;


(* problem 8*)
let rec change : int list -> int -> int
= fun coins amount -> 
if amount=0 then 1
else if amount<0 then 0
else (match coins with
		|[]->0
		|hd::tl->(change tl amount)+(change coins (amount-hd)));;