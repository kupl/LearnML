(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
if n = 0 then 1 else
(if n mod 2 = 0 
	then fastexpt b (n/2) * fastexpt b (n/2) 
else fastexpt b (n-1) * b);;

(* problem 2*)
let rec small divnow n =
	if n mod divnow =0 then divnow
	else small (divnow+1) n;;
let smallest_divisor : int -> int
= fun n ->
small 2 n;;



(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
match n with
 0-> fun x -> x
|_-> fun x -> iter((n-1),f) (f x);;


(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> 
if a > b then 1
else (f a) * (product f (a+1) b);;

(* problem 5*)

let dfact : int -> int
= fun n -> 
if n mod 2 =0 then product (fun x -> 2*x) 1 (n/2)
else product (fun x -> 2*x +1) 0 (n/2);;


(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n ->
match l with
[]->[]
|_::t ->if n=1 then t else drop t (n-1);;

(* =  problem 7*)
let fst(x,_) = x;;
let snd(_,x) = x;;

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
match lst with
[] -> ([],[])
| h::t -> (fst(h)::fst(unzip t), snd(h)::snd(unzip t));;

(* problem 8*)


let rec tree lst sum amount= 
if sum=amount then 1
else if sum>amount then 0
else
 match lst with
 []->0
 |hd::tl -> (tree lst (sum+hd) amount)+ (tree tl sum amount);;

let change : int list -> int -> int
= fun coins amount ->

if coins = [] then 0
else if amount = 0 then 1
else if amount <0 then 0
else tree coins 0 amount;;


