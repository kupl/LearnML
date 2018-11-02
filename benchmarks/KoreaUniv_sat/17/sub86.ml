(* problem 3*)
type  formula = True | False | Var of string | Neg of formula | And of formula * formula | Or of formula * formula | Imply of formula * formula | Iff of formula * formula
let rec sat : formula -> bool = fun f ->
     match f with
       True -> true
      |False -> false
      |Var p -> true
      |Neg p -> if sat p == true then false else true
      |And (p, q) -> sat p && sat q
      |Or (p, q) -> sat p || sat q    
      |Imply (p, q) -> if (sat p == true) && (sat q == false) then false else true
      |Iff (p, q) -> if sat p == sat q then true else false;;
