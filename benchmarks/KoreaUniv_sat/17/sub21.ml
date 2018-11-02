(*problem 3*)

type formula =
True
| False
| Var of string
| Neg of formula
| And of formula * formula
| Or of formula * formula
| Imply of formula * formula
| Iff of formula * formula

let rec sat
  = fun f-> match f with
    |True->true
      |False->false
  |Var x -> true
  |Neg(x) -> false
     |And (x,y)-> (sat x) &&(sat y)
    |Or (x,y) -> (sat x) ||(sat  y)
    |Imply(x,y) -> not(sat x) ||(sat y)
    |Iff(x,y) -> (sat x) = (sat y)