type formula = True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp = Num of int
| Plus of exp * exp
| Minus of exp * exp

let rec eval2 = fun a ->
   match a with
   | Num a -> a
   | Plus(exp_1, exp_2) ->
       let num1 = eval2 exp_1 in
       let num2 = eval2 exp_2 in
       num1 + num2
   | Minus(exp_1, exp_2) ->
       let num1 = eval2 exp_1 in
       let num2 = eval2 exp_2 in
       num1 - num2

let rec eval = fun a ->
   match a with 
   | True -> true
   | False -> false
   | Not formula_1 -> 
       let b = eval formula_1 in
       if b = true then false
       else true
   | AndAlso(formula_1, formula_2) ->
       let bool1 = eval formula_1 in
       let bool2 = eval formula_2 in
       bool1 && bool2 
   | OrElse(formula_1, formula_2) -> 
       let bool1 = eval formula_1 in
       let bool2 = eval formula_2 in
       bool1 || bool2
   | Imply(formula_1, formula_2) -> 
       let bool1 = eval formula_1 in
       let bool2 = eval formula_2 in
       if (bool1 = true && bool2 = false) then false
       else true
   | Equal(exp_1 ,exp_2) ->
       let num1 = eval2 exp_1 in
       let num2 = eval2 exp_2 in
       if num1 = num2 then true
       else false


