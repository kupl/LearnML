(* problem 1*)
let is_even x = x mod 2 = 0;;

let rec fastexpt : int -> int -> int
= fun b n -> 
if n = 0 then 1
else if n = 2 then b * b
else if is_even n then (fastexpt b (n/2)) * (fastexpt b (n/2))
else b * (fastexpt b (n-1));;

(* problem 2*)
let rec divisorble i n =
if n mod i = 0 then i
else if i = int_of_float(sqrt(float_of_int(n))) then n
else divisorble (i+1) n;;

let smallest_divisor : int -> int
= fun n -> divisorble 2 n;;

(* problem 3*)

let id x = x;;
let compose f g = fun x -> f(g(x));;
let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
if n = 0 then id
else if n = 1 then f
else compose f (iter(n-1,f));;

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> 
if b - a = 0 then f a
else (f a)*(product f (a+1) b);;

(* problem 5*)
let even x =
if is_even x then x
else 1;;

let odd x =
if is_even x then 1
else x;;

let dfact : int -> int
= fun n -> 
if n = 0 then 1
else if is_even n then product even 1 n
else product odd 1 n;;

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> 
match l with
| [] -> []
| hd :: tl ->
if n = 0 then l
else if n = 1 then tl
else drop tl (n-1);;

(* problem 7*)
let rec decompose_a l =
match l with
| [] -> []
| (a,_)::tl -> [a]@(decompose_a tl);;

let rec decompose_b l =
match l with
| [] -> []
| (_,b)::tl -> [b]@(decompose_b tl);;

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
match lst with
| [] -> ([], [])
| (a,b)::tl -> ([a]@(decompose_a tl), [b]@(decompose_b tl));;

(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> 
match coins with
| [] -> 0
| hd::tl ->
if amount = 0 then 1
else if amount < 0 then 0
else (change tl amount) + (change coins (amount-hd));;
