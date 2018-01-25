(*		problem 1		*)
let rec max : int list -> int
   = fun lst ->
     match lst with
     | [] -> 0
     | hd::[] -> hd
     | hd::tl ->
         let n = max tl in
           if hd < n then n
           else hd
 
 
let rec min :int list -> int
   = fun lst ->
     match lst with
     | [] -> 0
     | hd::[] -> hd
     | hd::tl ->
         let n = min tl in
           if hd > n then n
           else hd

(*    problem 2   *)
let rec filter pred lst =
   match lst with
   |[] -> []
   |hd::tl ->
       if pred hd == true then hd::filter pred tl
       else filter pred tl


(*    problem 3   *)
let rec double f a =
	f (f a)

(*    problem 4   *)
type btree =
  |Empty
  |Node of int * btree * btree

let rec mem : int -> btree -> bool
  = fun n tree ->
    match tree with
    |Node (y, left, right)  ->
				n==y || mem n left || mem n right
    |Empty -> false

(*    problem 5   *)
type nat =
   | ZERO
   | SUCC of nat
 
let rec natadd : nat -> nat -> nat
   = fun n1 n2 ->
       match n1 with
       | ZERO -> n2
       | SUCC n1 -> SUCC(natadd n1 n2)
 
let rec natmul : nat -> nat -> nat
   = fun n1 n2 ->
     match n1 with
     | ZERO -> n2
     | SUCC n1 -> SUCC(natmul n1 (natmul n1 n2))   

(*    problem 6   *)
type formula =
   | True
   | False
   | Not of formula
   | AndAlso of formula * formula
   | OrElse of formula * formula
   | Imply of formula * formula
   | Equal of exp * exp

and exp =
   | Num of int
   | Plus of exp * exp
   | Minus of exp * exp
 
let rec eval : formula -> bool
   = fun f ->
       match f with
       | True -> true
       | False -> false
       | Not (a) -> not (eval a)
       | AndAlso (a, b) -> eval a && eval b
       | OrElse (a, b) -> eval a || eval b
       | Imply (a, b) -> if eval a then eval b else true
       | Equal (a, b) -> a = b
         
let rec expr
		= fun e ->
       match e with
       | Num (n) -> n
       | Plus (e1, e2) -> (expr e1) + (expr  e2)
       | Minus (e1, e2) -> (expr e1) - (expr  e2)

