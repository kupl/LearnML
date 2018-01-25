(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> 
if n= 0 then 1
else if n= 1 then b
else let c = n/2 
		in (if n mod 2 =0 then 1 else b)*(fastexpt b c)*(fastexpt b c)
	 

(* problem 2*)

let smallest_divisor : int -> int
= fun n -> let rec loop n x = 
	if float_of_int x > sqrt (float_of_int n) then n (*prime number*)
	else if (n mod x =0) then x
	else loop n (x+1)
		in loop n 2 


(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> let compose (f, g) (x) = f (g x) in
if n > 0 then compose (f, iter(n-1, f))
else fun x -> x


(* problem 4*)

let rec product : (int ->int) -> int -> int -> int
= fun f a b ->  
if a>b then 1 else (f b)*product f a (b-1)

(* problem 5*)

let dfact : int -> int
= fun n -> if n mod 2=0 then product (fun x -> x*2) 1 (n/2)
else product (fun x -> 2*x-1) 1 ((n+1)/2)



(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> match l with
|[]-> []
|hd::tl -> if n=0 then hd::tl else drop tl (n-1)



(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> match lst with
| [] -> ([],[])
| (a,b)::tl -> let (x,y) = unzip tl in (a::x, b::y)



(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> let rec reverse l = match l with
|[] -> []
|hd::tl -> (reverse tl) @ hd ::[] in 
if amount = 0 then 1
else if amount < 0 then 0
else match reverse coins with
|[]-> 0
|head::tail -> change coins (amount-head) + change tail amount


