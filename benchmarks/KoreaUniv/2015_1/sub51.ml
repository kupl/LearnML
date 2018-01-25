(* Problem 1 *)
let rec pascal (x,y) = 
if y = 0 || y = x then 1
else pascal (x-1,y-1) + pascal (x-1,y);;

(* Problem 2 *)
let rec sigma f a b = 
if a < b then ((f b) + (sigma f a (b-1)))
else f b;;

(* Problem 3 *)
let rec max l =
match l with
[] -> raise (Failure "Empty List")
|[hd] -> hd
|hd::tl -> if (hd > (max tl)) then hd else (max tl);;

let rec min l =
match l with
[] -> raise (Failure "Empty List")
|[hd] -> hd
|hd::tl -> if (hd < (min tl)) then hd else (min tl);;

(* Problem 4 *)
type formula =
True
| False
| Neg of formula
| Or of formula * formula
| And of formula * formula
| Imply of formula * formula
| Equiv of formula * formula;;

(*
let eval formula = 
match formula with
True -> true
|False -> false
| Neg x -> if x = False then true else false
| Or (x,y) -> if (x = False && y = False) then false else true 
| And (x,y) -> if (x = True) && (y = True) then true else false
| Imply (x,y) -> if (x = True) && (y = False) then false else true
| Equiv (x,y) -> if x = y then true else false;;
*)

let rec eval formula = 
match formula with
True -> true
|False -> false
| Neg x -> not (eval x)
| Or (x,y) -> (eval x) || (eval y)
| And (x,y) -> (eval x) && (eval y)
| Imply (x,y) -> not (eval x) || (eval y)
| Equiv (x,y) -> if eval x = eval y then true else false;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat

let rec natadd n1 n2 =  
match n1 with
ZERO -> n2
|SUCC nat -> natadd nat (SUCC n2);;

let rec natmul n1 n2 =
match n1 with
ZERO -> ZERO
|SUCC nat -> if nat = ZERO then n2 else natmul nat (natadd n2 n2);;