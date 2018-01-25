(* problem 1 *)
let rec fastexpt : int -> int -> int = fun b n ->
if n=0 then 1
else if n=1 then b
else if n=2 then b*b
else if n mod 2 = 0 then (fastexpt b (n/2)) * (fastexpt b (n/2))
else (fastexpt b (n/2)) * (fastexpt b ((n/2)+1))

(* problem 2 *)
let smallest_divisor : int -> int = fun n ->
let rec loop i = if (n mod 2)=0 then 2
else if (n mod i)=0 then i
else loop (i+1) in loop 2
(*
(* problem 3 *)
let iter : int * (int -> int) -> (int -> int) = fun (n,f) ->

(* problem 4 *)
let product : (int -> int) -> int -> int -> int = fun f a b ->
*)
(* problem 5 *)
let rec dfact : int -> int = fun n ->
if (n=0)||(n=1) then 1
else n * dfact (n-2)

(* problem 6 *)
let rec length l =
match l with
|[] -> 0
|hd::tl -> 1 + (length tl)

let rec drop : 'a list -> int -> 'a list = fun l n -> 
match l with
|[] -> []
|hd::tl -> if n=0 then l
else if n>=length l then []
else drop tl (n-1)

(*
(* problem 7 *)
let unzip : ('a * 'b) list -> 'a list * 'b list = fun lst ->

(* problem 8 *)
let change : int list -> int -> int = fun coins amount ->
*)
