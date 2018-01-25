(* Problem 1 *) 
let rec pascal : int * int -> int =fun (x,y) ->
  if y=0 || y=x then 1
  else pascal(x-1,y)+pascal(x-1,y-1)

(* Problem 2 *) 
let rec sigma : (int -> int) -> int -> int -> int =fun f a b ->  
  if a=b then 1
  else
  (sigma f a (b-1)) + (f b)

(* Problem 3 *) 
let rec max : int list -> int =fun l -> match l with
  [] -> 0
  |h::t-> 
  if h>max t then h
  else max t

let rec min : int list -> int =fun l -> match l with
  [] -> 0 
  |h::t -> 
  if h>min t && t!=[] then min t 
  else h

(* Problem 4 *) type formula = 
True | False  | Neg of formula  | Or of formula * formula  
| And of formula * formula  | Imply of formula * formula
| Equiv of formula * formula
let rec eval : formula -> bool = fun f -> 
match f with
 True -> true
|False -> false
|Neg x -> if x = True then eval False else eval True
|Or (x,y) -> if x = True then eval True else if y = True then eval True else eval False
|And (x,y) -> if x= True && y=True then eval True else eval False
|Imply (x,y) -> if x != y then eval True else eval False
|Equiv (x,y) -> if x = y then eval True else eval False

(* Problem 5 *) 
type nat = ZERO | SUCC of nat 
let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
match n1 with
ZERO -> n2
| SUCC n1 -> SUCC (natadd n1 n2)

let rec natmul : nat -> nat-> nat
  = fun n1 n2 ->
  match (n1,n2) with
  (ZERO,_)-> ZERO | (_,ZERO) -> ZERO
  | (SUCC ZERO,_) -> n2
  | (SUCC n1, n2) -> (natadd (natmul n1 n2) n2)