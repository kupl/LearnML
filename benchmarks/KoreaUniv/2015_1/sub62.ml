(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) ->
if y>x then raise (Failure "wrong input location")
  else if y=0 then 1
  else if y=x then 1
  else pascal(x-1,y-1) + pascal(x-1,y);;


(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->
  if a>b then 0 else
  (f a) + (sigma f (a+1) b);;


(* Problem 3 *)
let rec max : int list -> int
=fun l -> 
match l with
[] -> raise (Failure "list is too short")
| h::t->if t=[] then h else if h > max t then h else max t;;


let rec min : int list -> int
=fun l -> 
match l with
[] -> raise (Failure "list is too short")
| h::t->if t=[] then h else if h < max t then h else max t;;


(* Problem 4 *)
type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f ->
  match f with
  True -> true
  |False -> false
  |Neg a -> not(eval a)
  |Or (a,b) -> eval(a) || eval(b)
  |And (a,b) -> eval(a)&&eval(b)
  |Imply (a,b) -> not(eval a) || eval(b)
  |Equiv (a,b) -> if eval a=eval b then true else false;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->
  match n1 with
  ZERO->n2
  |SUCC(h) -> SUCC(natadd h n2);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
  match n1 with
  ZERO->ZERO
|SUCC ZERO -> n2
|SUCC(h)->(natadd n2 (natmul h n2));;
