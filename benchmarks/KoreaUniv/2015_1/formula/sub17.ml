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
=fun f -> match f with
| True -> true | False -> false | 
Neg True->false | Neg False -> true |
Or (True,True)->true |Or (True,False)->true | Or(False,True)->true| Or(False,False)->false
|And (True,True)->true | And(True,False)->false | And(False,True)->false | And(False,False)->false
|Imply (True,True)->true | Imply(True,False)->false | Imply(False,True)->true | Imply(False,False)->true
|Equiv (True,True)->true | Equiv(True,False)->false | Imply(False,True)->false| Imply(False,False)->true;;
