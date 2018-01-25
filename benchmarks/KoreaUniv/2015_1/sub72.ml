(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 
  match (x,y) with
  | (_, 0) -> 1
  | _ -> 
    if x=y then 1
    else pascal(x-1, y) + pascal(x-1, y-1)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
  if a<=b then (f a) + (sigma f (a+1) b)
  else 0

(* Problem 3 *)
let rec max : int list -> int
=fun l ->   
  let rec compare : int -> int -> int 
  =fun a b ->
    if a > b then a
    else b
  in if (List.length l) > 1 then
    if (List.hd l) > (List.hd (List.tl l)) then compare (List.hd l) (max (List.tl l))   
    else max (List.tl l)
  else List.hd l

let rec min : int list -> int
=fun l ->   
  let rec compare : int -> int -> int 
  =fun a b ->
    if a < b then a
    else b
  in if (List.length l) > 1 then
    if (List.hd l) < (List.hd (List.tl l)) then compare (List.hd l) (min (List.tl l))   
    else min (List.tl l)
  else List.hd l

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
  | Neg f1 -> not (eval f1)
  | Or (f1, f2) -> eval f1 || eval f2
  | And (f1, f2) -> eval f1 && eval f2 
  | Imply (f1, f2) -> if (eval f1)==true && (eval f2)==false then false else true 
  | Equiv (f1, f2) -> if f1==f2 then true else false 
       
(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
  match (n1, n2) with
  | (ZERO, ZERO) -> ZERO
  | (ZERO, SUCC nat1) -> SUCC (natadd ZERO nat1)
  | (SUCC nat1, ZERO) -> SUCC (natadd nat1 ZERO)
  | (SUCC nat1, SUCC nat2) -> SUCC(SUCC(natadd nat1 nat2))  

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
  let rec count : nat -> int
  =fun n ->
    match n with
    | ZERO -> 0
    | SUCC a -> 1 + count a 
  in 
  let rec natinit : int -> nat
  =fun i ->
    if i > 0 then SUCC (natinit (i-1))
    else ZERO 
  in
  natinit ((count n1) * (count n2))
