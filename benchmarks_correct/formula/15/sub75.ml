type formula = TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

let rec eval2 = fun a ->
   match a with
   | NUM a -> a
   | PLUS(expr_1, expr_2) ->
       let num1 = eval2 expr_1 in
       let num2 = eval2 expr_2 in
       num1 + num2
   | MINUS(expr_1, expr_2) ->
       let num1 = eval2 expr_1 in
       let num2 = eval2 expr_2 in
       num1 - num2

let rec eval = fun a ->
   match a with 
   | TRUE -> true
   | FALSE -> false
   | NOT formula_1 -> 
       let b = eval formula_1 in
       if b = true then false
       else true
   | ANDALSO(formula_1, formula_2) ->
       let bool1 = eval formula_1 in
       let bool2 = eval formula_2 in
       bool1 && bool2 
   | ORELSE(formula_1, formula_2) -> 
       let bool1 = eval formula_1 in
       let bool2 = eval formula_2 in
       bool1 || bool2
   | IMPLY(formula_1, formula_2) -> 
       let bool1 = eval formula_1 in
       let bool2 = eval formula_2 in
       if (bool1 = true && bool2 = false) then false
       else true
   | LESS(expr_1 ,expr_2) ->
       let num1 = eval2 expr_1 in
       let num2 = eval2 expr_2 in
       if num1 < num2 then true
       else false


