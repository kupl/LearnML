(* problem 1*)
let fastexpt : int -> int -> int
= fun b n -> let rec loop n = match n with 
0 -> 1
| _ -> if n mod 2 = 0 then (fun x -> x*x) (loop (n/2))
else b*(loop (n-1))
in (loop n)

(* problem 2*)

let smallest_divisor : int -> int
= fun n -> let rec loop x = 
if x > int_of_float (sqrt (float_of_int n)) then n
else if n mod x == 0 then x
else (loop (x+1))
in (loop 2)

(* problem 3*)

let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> fun k -> let rec loop x =
if x = 0 then k
else f (loop (x-1)) 
in (loop n)

(* problem 4*)

let product : (int -> int) -> int -> int -> int
= fun f a b -> let rec loop x =
if x > b then 1
else (f x)*(loop (x+1))
in (loop a)

(* problem 5*)

let dfact : int -> int
= fun n -> if n mod 2 = 0 then product (fun x->2*x) 1 (n/2)
else product (fun x->2*x-1) 1 ((n+1)/2)

(* problem 6*)

let drop : 'a list -> int -> 'a list
= fun l n -> let rec loop x y =
match x with
| [] -> []
| hd::tl -> if y = 0 then x
else (loop tl (y-1))
in (loop l n)

(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> let rec loop lst = 
match lst with
| [] -> ([],[])
| hd::tl -> (fun (x,y) (a,b) -> (x::a,y::b)) hd (loop tl)
in (loop lst)

(* problem 8*)

let change : int list -> int -> int
= fun coins amount -> let rec loop coins amount =
match coins with
| [] -> 0
| hd::tl -> if amount < 0 then 0 
else if amount = 0 then 1
else (loop tl amount) + (loop coins (amount - hd))
in (loop coins amount)
