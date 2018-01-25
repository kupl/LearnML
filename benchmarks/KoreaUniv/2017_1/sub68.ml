exception Problem


(*problem1*)
let rec fastexpt:int->int->int = fun b n ->
	if n=0 then 1
	else if n<0 then raise Problem
	else if (n mod 2 =0) then ((fastexpt b (n/2))*(fastexpt b (n/2)))
	else (b*(fastexpt b (n-1)))



(*problem2*)

let rec smallest_divisor_sub : int->int->int
=fun a b->
if((b*b)>a) then a
else if a<2 then raise Problem
else if(a mod b=0) then b
else smallest_divisor_sub a (b+1)
let smallest_divisor : int->int 
= fun a->
smallest_divisor_sub a 2

(*problem3*)
let rec iter : int*(int->int)-> (int-> int)
= fun (n,f) ->
if n>0 then fun x -> f(iter((n-1),f) x)
else if n=0 then fun x-> x
else raise Problem;;

(*problem4*)

let rec product : (int->int) -> int-> int-> int 
= fun f a b->
if a<b then (f a)*(product f (a+1) b)	
else if a=b then f a
else raise Problem

(*problem 5*)

let rec dfact : int -> int
= fun n->
if (0<=n && n<=1) then 1
else if n>1 then n*dfact (n-2)
else raise Problem


(*problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n->
if n>0 then 
match l with
| [] -> drop l (n-1)
| hd::tl -> drop tl (n-1)
else if n<0 then raise Problem
else l

(*problem 7*)
let rec unzip_a :('a*'b) list -> 'a list
= fun lst ->
match lst with
| [] -> []
| (x,y)::tail -> x::unzip_a tail

let rec unzip_b :('a*'b) list -> 'b list
= fun lst ->
match lst with
| [] -> []
| (x,y)::tail -> y::unzip_b tail

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
match lst with
| [] -> ([],[])
|_-> (unzip_a lst , unzip_b lst)


(*problem 8*)

let rec change : int list -> int -> int
=fun coins amount ->
match coins with 
| []-> if amount=0 then 1
	   else 0
| hd::tl -> if amount=0 then 1
		   	else if amount >0 then change coins (amount-hd) + change tl amount
		   	else 0
