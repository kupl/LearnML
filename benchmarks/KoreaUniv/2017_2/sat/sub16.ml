(*Problem 3*)
type formula =
  True
| False
| Var of string
| Neg of formula
| And of formula * formula
| Or of formula * formula
| Imply of formula * formula
| Iff of formula * formula

let rec ftbt : formula -> bool
 = fun f -> match f with
|True -> true
|False -> false
|Var string -> true
|Neg f -> not (ftbt f)
|And (f1, f2) -> (ftbt f1) && (ftbt f2)
|Or (f1, f2) -> (ftbt f1) || (ftbt f1)
|Imply (f1, f2) -> if ((ftbt f1) = false) then true else ftbt f2
|Iff (f1, f2) -> if ((ftbt f1) = (ftbt f2)) then true else false

let rec ftbf : formula -> bool
= fun f ->match f with
|True -> true
|False -> false
|Var string -> false
|Neg f -> not (ftbf f)
|And (f1, f2) -> (ftbf f1) && (ftbf f2)
|Or (f1, f2) -> (ftbf f1) || (ftbf f1)
|Imply (f1, f2) -> if ((ftbf f1) = false) then true else ftbf f2
|Iff (f1, f2) -> if ((ftbf f1) = (ftbf f2)) then true else false

let rec ftbtf : formula -> bool
= fun f ->match f with
|True -> true
|False -> false
|Var string -> true
|Neg f -> not (ftbtf f)
|And (f1, f2) -> (ftbt f1) && (ftbf f2)
|Or (f1, f2) -> (ftbt f1) || (ftbf f1)
|Imply (f1, f2) -> if ((ftbt f1) = false) then true else ftbf f2
|Iff (f1, f2) -> if ((ftbt f1) = (ftbf f2)) then true else false
 
let rec ftbft : formula -> bool
= fun f ->match f with
|True -> true
|False -> false
|Var string -> true
|Neg f -> not (ftbft f)
|And (f1, f2) -> (ftbf f1) && (ftbt f2)
|Or (f1, f2) -> (ftbf f1) || (ftbt f1)
|Imply (f1, f2) -> if ((ftbf f1) = false) then true else ftbf f2
|Iff (f1, f2) -> if ((ftbf f1) = (ftbt f2)) then true else false 

let rec getStringfront : formula -> string
= fun f ->match f with
|True -> "True"
|False -> "False"
|Var string -> string
|Neg f -> getStringfront f
|And (f1, f2) -> getStringfront f1
|Or (f1, f2) -> getStringfront f1
|Imply (f1, f2) -> getStringfront f1
|Iff (f1, f2) -> getStringfront f1

let rec getStringlast : formula -> string
 = fun f ->match f with
|True -> "True"
|False -> "False"
|Var string -> string
|Neg f -> getStringlast f
|And (f1, f2) -> getStringlast f2
|Or (f1, f2) -> getStringlast f2
|Imply (f1, f2) -> getStringlast f2
|Iff (f1, f2) -> getStringlast f2




let sat : formula -> bool
= fun f -> if(getStringfront f = getStringlast f) then (if (ftbt f = false && ftbf f = false) then false else true)
  else (if (ftbt f = false && ftbf f = false && ftbtf f = false && ftbft f = false) then false
else true)
