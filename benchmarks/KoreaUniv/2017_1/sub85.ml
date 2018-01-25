(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
 if n = 0 then 1
else if (n mod 2) = 0 then (fun x -> x * x) (fastexpt b (n/2))
else b * (fastexpt b (n-1));; 

(* problem 2*)

let smallest_divisor : int -> int
= fun n ->
 let rec find_divisor n d =
if (n mod d) = 0 then d
else if (fun x -> x*x) d > n then n
else find_divisor n (d+1)
in find_divisor n 2;;

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
if n = 0 then (fun x -> x) else
(fun f g x -> f(g x)) f (iter (n-1,f));;

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
if (b-a = 0) then f a
else (f a) * product f (a+1) b;;

(* problem 5*)

let dfact : int -> int
= fun n ->
if (n mod 2 = 0) then product (fun x -> 2*x) 1 (n/2)
else product (fun x -> 2*x-1) 1 ((n+1)/2);;

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n ->
match l with
|[] -> []
|hd::tl -> if n = 0 then l else drop tl (n-1);;

(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
let rec map f l =
match l with
|[] -> []
|hd::tl -> (f hd)::(map f tl)
in (map fst lst, map snd lst);;

(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount ->
let rec sort l =
match l with 
|[] -> []
|hd::tl ->
let rec insert a l =
match l with
|[] -> [a]
|hd::tl ->
if a > hd then a::hd::tl
else hd::(insert a tl) in
insert hd (sort tl) in
match (sort coins) with
|[] -> 0
|hd::tl ->
if amount = 0 then 1 else
if amount < 0 then 0 else
change (sort coins) (amount-hd) + change tl amount;;
