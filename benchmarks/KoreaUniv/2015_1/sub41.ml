(* Problem 1 *)
let rec pascal (x, y) = 
if y = 0 then 1
else if x = y then 1 
else pascal(x-1, y-1) + pascal(x-1, y);;

(* Problem 2 *)
let rec sigma func a b =
if a>b then func b
else if a<b then (func b) + (sigma func a (b-1))


(* Problem 3 *)
let rec max l =
match l with
[] -> []
| [hd] -> hd
| hd::tl -> if hd > max tl then hd else max tl;;
let rec min l =
metch l with
[] -> []
| [hd] -> hd
| hd::tl -> if hd < min tl then hd else min tl;;


(* Problem 4 *)
type formula =
True
| False
| Neg of formula
| Or of formula * formula
| And of formula * formula
| Imply of formula * formula
| Equiv of formula * formula

let rec eval formula =
match formula with
True -> true
| False -> false
| Neg x -> if x = False then true else false
| Or (x,y) -> if x = False && y = False then false else true 
| And (x,y) -> if x = True && y = True then true else false
| Imply (x,y) -> if x = True && y = False then false else true
| Equiv (x,y) -> if (x = True && y = True) || (x = False && y = False) then true else false;;

(* Problem 5 *)
type nat = ZERO | SUCC of nat
let rec natadd nat1 nat2 =
match nat1 with
ZERO -> nat 2
|SUCC nat -> natadd nat (SUCC nat2);;
let rec natmul nat1 nat2 = 
match nat1 with
ZERO -> ZERO
|SUCC nat -> natmul nat (natadd nat2 nat2);;

