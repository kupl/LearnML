(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 
  if y=0 then 1
  else if x=y then 1
  else pascal (x-1,y-1) + pascal (x-1,y)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
  if a>b then 0
  else sigma f (a+1) b + f a

(* Problem 3 *)
let rec max : int list -> int
=fun l -> 
  match l with
  | [] -> raise (Failure "Empty list")
  | [a] -> a
  | hd::tl ->
    let maxintl = max tl in
    if maxintl < hd then hd
    else maxintl

let rec min : int list -> int
=fun l ->
  match l with
  | [] -> raise (Failure "Empty list")
  | [a] -> a
  | hd::tl ->
    let minintl = min tl in
    if minintl > hd then hd
    else minintl

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
  | True -> true
  | False -> false
  | Neg a -> eval a = false
  | Or (a,b) -> eval a || eval b
  | And (a,b) -> eval a && eval b
  | Imply (a,b) -> if eval a then eval b else true
  | Equiv (a,b) -> eval a = eval b

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->
  match n2 with
  | ZERO -> n1
  | SUCC a -> natadd (SUCC n1) a

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> 
  match n2 with
  | ZERO -> ZERO
  | SUCC ZERO -> n1
  | SUCC a -> natadd (natmul n1 a) n1
