(* problem 1*)
let square x = x*x
let iseven x = (x mod 2) = 0
exception Input_range

let rec fastexpt : int -> int -> int
= fun b n -> (*TODO*)
	if n<0 then raise Input_range
	else if n=0 then 1
	else if iseven n then square (fastexpt b (n/2))
	else b * (fastexpt b (n-1))



(* problem 2*)
let abs n = if n > 0 then n else (-n)
let isdivisor n test = (n mod test) = 0
let rec findodd n odd = 
	if (odd*odd) > n then n
	else if isdivisor n odd then odd
	else findodd n (odd+2)

let smallest_divisor : int -> int
= fun n -> 
	if n=0 || n = 1 then n
	else if isdivisor n 2 then 2
	else findodd (abs n) 3



(* problem 3*)
let compose f g = fun x -> f(g(x))

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
	if n<0 then raise Input_range
	else if n = 0 then (fun x->x) 
	else if n=1 then f else compose f (iter ((n-1), f))




(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> 
	if a > b then raise Input_range
	else if a = b then f a
	else (f a)*(product f (a+1) b)

let fact n = if n=0 then 1 else product (fun x -> x) 1 n



(* problem 5*)

let dfact : int -> int
= fun n -> 
	if n =0 then 1
	else if iseven n then product (fun x->2*x) 1 (n/2)
	else product (fun x ->((2*x)-1)) 1 ((n+1)/2)



(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> 
	if n <= 0 then l
	else match l with
		 | [] -> []
		 | hd::tl -> drop tl (n-1)




(* problem 7*)
let fst (x,_) = x
let snd (_,x) = x

let rec mkl f l =
	match l with
	| []->[]
	| hd::tl -> (f hd)::(mkl f tl)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
	(mkl fst lst, mkl snd lst)




(* problem 8*)
let rec insert a l = 
	match l with 
	| [] -> [a]
	| hd::tl -> if a > hd then a::hd::tl else hd::(insert a tl)

let rec sort l =
	match l with
	| [] -> []
	| hd::tl -> insert hd (sort tl)

let rec calc coins amount = 
	if amount = 0 then 1
	else if amount <0 then 0
	else match coins with
	| [] -> 0
	| hd::tl -> if hd <= 0 then 0 
		else ((calc tl amount) + (calc coins (amount-hd)))

let change : int list -> int -> int
= fun coins amount -> 
	if amount = 0 then 1
	else if amount < 0 then 0
	else if coins = [] then 0
	else calc (sort coins) amount


