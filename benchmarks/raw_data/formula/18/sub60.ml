type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp
  

and exp = 
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp
  
  
   let rec cal exp =
    match exp with
      Plus(exp1,exp2) -> cal exp1 + cal exp2
      |Minus(exp1,exp2) -> cal exp1 - cal exp2
      |Num n -> n
  ;;

let rec eval : formula -> bool
= fun f -> (*TODO*)
match f with 
  True-> true
  |False-> false
  |Not(formula)->if eval formula = true then false else true
  |AndAlso(formula1,formula2) ->if ((eval formula1) && (eval formula2)) = true then true else false
  |OrElse(formula1,formula2)->if eval formula1 || eval formula2 = true then true else false
  |Imply(formula1,formula2) -> if eval formula1 = true&& eval formula2 = false then false else true
  |Equal(exp1,exp2) -> if cal exp1 = cal exp2 then true else false 
  ;;
  

 
  
  