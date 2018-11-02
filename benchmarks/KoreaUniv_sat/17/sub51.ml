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

let rec form : formula -> formula = fun f ->
match f with
|True -> True
|False -> False
|Var x -> Var x
|Neg f1 -> 
  (match f1 with
  |Neg f2 -> form f2
  |_ -> if (form f1 = True) then False else if (form f1 = False) then True else Neg (form f1))
|And (f1,f2) ->
  if (form f1 = form (Neg f2)) then False
  else if (form f1 = False && form f2 = False) then False
  else True
|Or (f1,f2) -> if (form f1 = False && form f2 = False) then False else True
|Imply (f1,f2) -> if (form f1 = True && form f2 = False) then False else True
|Iff (f1,f2) -> if (form f1 = form f2) then True else False;;
  
let rec sat : formula -> bool = fun f ->
match f with
|True -> true
|False -> false
|Var x -> true
|_ -> sat (form f);;
