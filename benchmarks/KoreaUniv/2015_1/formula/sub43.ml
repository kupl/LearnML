type formula = 
True | False  | Neg of formula  | Or of formula * formula  
| And of formula * formula  | Imply of formula * formula
| Equiv of formula * formula
let rec eval : formula -> bool = fun f -> 
match f with
 True -> true
|False -> false
|Neg x -> if x = True then eval False else eval True
|Or (x,y) -> if x = True then eval True else if y = True then eval True else eval False
|And (x,y) -> if x= True && y=True then eval True else eval False
|Imply (x,y) -> if x != y then eval True else eval False
|Equiv (x,y) -> if x = y then eval True else eval False
