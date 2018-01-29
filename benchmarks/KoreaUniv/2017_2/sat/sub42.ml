(* problem 3*)
 type formula = True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

 let rec sat : formula -> bool
 = fun f ->
match f with
 True -> true
 |False -> false
 |Var a ->( match a with "P" -> true |_ -> false )
 |Neg s -> if s=True then false else true
 |Or (a, b) -> if (a=False)&&(b=False) then false else true
 |And (a, b) -> if (a=True)&&(b=True) then true else false
 |Imply (a, b) -> if a=True&&b=False then false else true
 |Iff (a, b) -> if a==b then true else false
