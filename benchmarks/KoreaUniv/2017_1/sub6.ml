(* problem 1*)
let rec fastexpt : int->int->int
  = fun b n -> 
  if n==0 then 1
  else if (n mod 2)==0 then (fastexpt b (n/2))*(fastexpt b (n/2))
  else (fastexpt b (n-1))*b;;

(* problem 2*)
let smallest_divisor : int->int
= fun n ->
let rec divisor a n =
if a==1 then n
else if (n mod a)==0 then a else divisor (a-1) n in
divisor (int_of_float (sqrt(float n))) n;;

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
let compose f g = fun x -> f(g(x)) in
if n==0 || n==1 then f
else compose f (iter (n-1,f));;

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
if a==b then f b
else (f b) * (product f a (b-1));;

(* problem 5*)

let dfact : int -> int
= fun n ->
let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
if a==b then f b
else (f b) * (product f a (b-1)) in
if (n mod 2)==0 then product (fun x->2*x) 1 (n/2)
else product (fun x->2*x-1) 1 ((n+1)/2);;

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n ->
if n==0 then l
else
match l with
| []->[]
| hd::tl -> drop tl (n-1);;

(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
let first_element tup = match tup with 
| (a,b)->a
in
let second_element tup = match tup with 
| (a,b)->b
in
match lst with
| [] -> ([],[])
| hd::tl -> ((first_element hd::first_element(unzip tl)), (second_element hd::second_element(unzip tl)));;

(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount ->
let rec last_del lit =
	match lit with
	| []->[]
	| [a]->[]
	| hd::tl -> hd::(last_del tl)
in
let rec last_ele lit =
	match lit with
	| []->0
	| [a]->a
	| hd::tl -> last_ele tl
in
if amount == 0 then 1
else if amount < 0 then 0
else if coins == [] then 0
else (change (last_del coins) amount) + (change coins (amount-(last_ele coins)));;

