(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> 
if n = 0 then 1
else if (n mod 2 = 0) then (fastexpt b (n/2)) * (fastexpt b (n/2))
else b * (fastexpt b (n-1))  

(* problem 2*)
let smallest_divisor : int -> int
= fun n -> 
let rec sd n i =
if (i*i)>n then n
else if (n mod i =0) then i
else sd n (i+1) in sd n 2
(* problem 3*)
let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
if n=0 then fun x->x
else if n=1 then f
else fun x-> iter (n-1 , f) (f x)

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> 
if a<b then (f a)*(product f (a+1) b)
else f a

(* problem 5*)

let rec dfact : int -> int
= fun n -> 
if n=0 || n=(-1) then 1
else n*(dfact (n-2))

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> (*TODO*)
match l with 
| [] -> []
| hd::tl -> if n>0 then hd::(drop tl (n-1))
else []
(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
match lst with
| [] -> ([],[])
| (r1, r2)::tl ->
match unzip tl with
| (l1, l2) -> (r1::l1, r2::l2)



(* problem 8*)

let rec change : int list -> int -> int 
= fun coins amount -> (*TODO*)
if (amount=0) then 1
else if amount<0 then 0 
else match coins with 
| hd::tl -> (change coins (amount-hd))+(change tl amount)
| [] -> 0 
