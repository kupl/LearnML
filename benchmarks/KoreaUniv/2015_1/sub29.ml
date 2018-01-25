(* Problem 1 *)
let pascal : int * int -> int
=fun (x,y) -> 
 let rec factorial a = 
   match a with
    0 -> 1
   |_ -> a*factorial(a-1)
 in factorial(x) / (factorial(y)*factorial(x-y))

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
 if b=a then f a else (f b) + sigma f a (b-1)

(* Problem 3 *)
let rec max : int list -> int
=fun l -> 
 match l with 
   [] -> raise (Failure "Empty List")
  |hd::[] -> hd
  |hd::tl -> let m = (max tl) in if hd > m then hd else m

let rec min : int list -> int
=fun l -> 
 match l with 
  [] -> raise (Failure "Empty List")
 |hd::[] -> hd
 |hd::tl -> let m = (min tl) in if hd < m then hd else m

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
 | False -> false
 | Neg x -> if (eval x) then false else true
 | Or (x,y) -> (eval x) || (eval y)
 | And (x,y) -> (eval x) && (eval y)
 | Imply (x,y) -> let cx = (eval x) in let cy = (eval y) in if cx = true then if cy = true then true else false else true
 | Equiv (x,y) -> let cx = (eval x) in let cy = (eval y) in if cx = cy then true else false

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->
 match n1 with
  ZERO -> n2
 |SUCC n -> SUCC (natadd n n2)
  

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> 
 match n2 with 
  ZERO -> ZERO
 |_ -> 
   match n1 with
    ZERO -> ZERO
   |SUCC n -> natadd (natmul n n2 ) n2
