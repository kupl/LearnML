(* problem 1*)
let square x = x*x;; (*helper function*)
let rec fastexpt:int->int->int = fun b n -> if b=0 then 0
else match n with |0->1|_->if n mod 2 = 0 then square (fastexpt b (n/2)) else b*(fastexpt b (n-1));;

(* problem 2*)
let rec smallest_helper n k = if (n mod k)=0 then k else if float_of_int k>sqrt(float_of_int n) then n else smallest_helper n (k+1);;
let smallest_divisor : int -> int
= fun n -> smallest_helper n 2;;

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> if n=0 then (fun x->x) else (fun x->f(iter(n-1,f) x));;

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b ->  if a=b then f(a) else f(b)*product f a (b-1);;

(* problem 5*)
let rec dfact : int -> int
= fun n -> if n = 0 then 0 else if n mod 2 = 0 then (product (fun x->2*x) 1 (n/2)) else (product (fun x->2*x+1) 0 (n/2));;
(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n ->if n=0 then l else match l with |[]->[]|hd::tl-> drop tl (n-1);;

(* problem 7*)

let rec makeList lst extract = 
match lst with |[]->[]|hd::tl->(extract hd)::makeList tl extract;;
let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> (makeList lst (fun tuple->match tuple with |(x,_)->x),
makeList lst (fun tuple->match tuple with |(_,y)->y));;

(* problem 8*)
let rec length l = match l with |[]->0|hd::tl->1+length tl;;
let rec change : int list -> int -> int
= fun coins amount -> match coins with |[]->0|hd::tl-> 
if (length coins)= 1 then 
(if amount=0 then 0 else if amount mod hd = 0 then 1 else 0)
else if amount>0 then ((change tl amount)+(change coins (amount-hd)))
else if amount=0 then 1 else 0;;

