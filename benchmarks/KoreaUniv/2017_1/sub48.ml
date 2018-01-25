(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
if n=0 then 1
else if (n mod 2)=0 then (fastexpt b (n/2)) * (fastexpt b (n/2))
else (fastexpt b (n/2)) * (fastexpt b (n/2)) * b

(* problem 2*)

let smallest_divisor : int -> int
= fun n -> 
let a = 2 in
let rec loop a n =
	if ((n mod a)=0) then a
else if ((a*a)>n) then n
else (loop (a+1) n)
in loop a n

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
if n=0 then (fun x->x)
else fun x -> f(iter(n-1, f) x)

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
if a<=b then (f a) * (product f (a+1) b)
else 1

(* problem 5*)

let dfact : int -> int
= fun n ->
if (n mod 2) = 0 then product (fun n -> n*2) 1 (n/2)
else product (fun n -> (n*2-1)) 1 ((n+1)/2)

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> 
if n<=0 then l else  
match l with
[] -> []
|h::t -> drop t (n-1)

(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
let fst p = match p with (x,_) -> x in
let snd p = match p with (_,x) -> x in
let rec unzip_prime = fun tl c d ->
match tl with
[] -> (c,d)
|h::t -> unzip_prime t (c@[fst h]) (d@[snd h])
 in unzip_prime lst [] []

(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount ->
if amount<0 then 0
else if amount=0 then 1 else
match coins with
[] -> 0
|h::t -> (change coins (amount-h))+(change t amount)