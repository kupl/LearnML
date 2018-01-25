(* problem 1. *)

let rec fastexpt : int -> int -> int
= fun b n ->
if n =1 then b
else if n mod 2 = 0 then (fastexpt b (n/2))*(fastexpt b (n/2))
else b*(fastexpt b (n-1))

(* problem 2. *)

let rec smallest_divisor : int -> int
= fun n ->

let rec find_divisor : int -> int -> int
= fun a b ->
if a mod b = 0 then b
else find_divisor a (b+2)

in
if n = 0 then 0
else if n mod 2 = 0  then 2
else find_divisor n 3

(*problem 3. *)

let rec iter : int * (int->int) -> (int->int)
= fun(n,f) ->

let rec iter_f f g x = 
f(g x)

in
if n = 0 then (fun x -> x)
else iter_f f (iter (n-1,f)) 

(*problem 4. *)

let rec product : (int->int) -> int -> int -> int
= fun f a b ->
if a = b then f a
else (f a) * (product f (a+1) b)

(*problem 5. *)

let rec dfact : int -> int
= fun n ->
if n = 1 then 1
else if n = 2 then 2
else n * (dfact (n-2)) 

(*problem 6. *)

let rec drop : 'a list -> int -> 'a list
= fun l n ->
if n = 0 then l
else
match l with
| [] -> []
| hd::tl -> drop tl (n-1)

(*problem 7. *)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
=fun lst ->

let rec pick1 l =
if l = [] then []
else
match l with
|[] -> []
|(hd1, hd2) ::tl -> hd1::(pick1 tl)

in
let rec pick2 l =
if l = [] then []
else
match l with
|[] -> []
|(hd1, hd2) ::tl -> hd2::(pick2 tl)

in
if lst = [] then ([],[])
else ((pick1 lst),(pick2 lst))

(*problem 8. *)

let rec change : int list -> int -> int
= fun coins amount ->

if amount = 0 then 1
else if amount < 0 then 0
else
match coins with
|[] -> 0
|hd::tl -> (change coins (amount-hd)) + (change tl amount) 




