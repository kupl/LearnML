(* Problem 1 *)
let rec pascal : int * int -> int = fun (x, y) ->
if x = y then 1
else if y = 0 then 1
else pascal(x-1, y-1) + pascal(x-1, y);;

(* Problem 2 *)
let rec sigma: (int->int)->int->int->int
= fun f a b ->
if a = b then f b
else f a + sigma f (a+1) b;;

(* Problem 3 *)
let first: int list -> int
= fun f ->
match f with
[] -> 0
|hd::tl -> hd;;

let switch: int * int list -> int list = fun (x, l) ->
match l with
[] -> x::[]
|hd::tl -> x::tl;;

let rec max: int list -> int = fun l ->
match l with
[] -> 0
|hd::tl -> if tl=[] then hd
else if hd > first tl then max(switch(hd, tl))
else max tl;;

let rec min: int list -> int = fun l ->
match l with
[] -> 0
|hd::tl -> if tl=[] then hd
else if hd < first tl then min(switch(hd,tl))
else min tl;;

(* Problem 4 *)
type formula=
True
|False
|Neg of formula
|Or of formula * formula
|And of formula * formula
|Imply of formula * formula
|Equiv of formula * formula;;

let rec eval: formula -> bool
= fun f ->
match f with
True -> true
|False -> false
|Neg f1 -> if eval f1=true then false else true
|Or (f1, f2) -> if eval f1 = false && eval f2 = false then false else true
|And (f1, f2) -> if eval f1 = true && eval f2 = true then true else false
|Imply (f1, f2) -> if eval f1 = true && eval f2 = false then false else true
|Equiv (f1, f2) -> if eval f1 = eval f2 then true else false;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd: nat -> nat -> nat
= fun n1 n2 ->
match n1 with 
ZERO -> n2
| (SUCC(num)) -> natadd num (SUCC(n2));;

let rec natmul: nat -> nat -> nat
= fun n1 n2 ->
if n1 = ZERO then ZERO
else if n2 = ZERO then ZERO
else
match n1 with
ZERO -> n2
| (SUCC (num)) -> natadd n2 ( natmul num n2);;
