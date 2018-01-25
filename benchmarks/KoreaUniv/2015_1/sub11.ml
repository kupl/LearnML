
(*2011120109 Jung-su Han Homework*)


(* Problem 1 *)
let pascal : int * int -> int
=fun (x,y) ->
  if x=y then 1
  else match (x,y) with
  (_,0) -> 1
  | _ ->let rec facto a=       
  if a=1 then 1 else a*facto(a-1) in
  (facto x)/((facto y)*(facto (x-y)));;

(* version that does not use recursive definition but uses factorial function and definition formula of pascal triangle *)



(* Problem 2 *)

let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->
  if a>b then raise (RangeError)
  else if a=b then f a
  else (f a)+(sigma f (a+1) b);;

(* defines exception for range being large number to smaller number *)




(* Problem 3 *)

let rec max : int list -> int
=fun l->
  match l with
  [] -> (min_int)
  |h::t -> if h>max t then h else max t;;

let rec min : int list -> int
=fun l ->
  match l with
  [] -> (max_int)
  |h::t -> if h<min t then h else min t;;

(* searched and used the existing variable max_int & min_int *)




(* Problem 4 *)

type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula;;

let rec eval : formula -> bool
=fun f -> match f with
          True -> true
          |False -> false
          |Neg(a) -> if eval a then false else true
  	  |Or (a,b) -> if eval a then true else if eval b then true else false
	  |And (a,b) -> if eval a then if eval b then true else false else false
	  |Imply (a,b)  -> if eval a then if eval b then true else false else true
	  |Equiv (a,b) -> if a=b then true else false;;


(* Problem 5 *)

type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
  let before (SUCC c) = if (SUCC c)=ZERO then ZERO else c in
  if n1 = ZERO then n2
  else SUCC(natadd (before n1) n2);;


let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
  let before (SUCC c) = if (SUCC c)=ZERO then ZERO else c in
  if n1 = ZERO then ZERO
  else natadd (natmul (before n1) n2) n2;;

(* ¡°before a¡± function returns the number before a(opposite of SUCC) *)
(* used recursive definition that ((a-1)+b)+1=(a+b) and ((a-1)*b)+b=(a*b) *)
