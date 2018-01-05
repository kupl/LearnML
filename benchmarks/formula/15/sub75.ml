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
       let bool = eval formula_1 in
       if bool = true then false
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

let _ = 
let print_bool x = 
print_endline (string_of_bool x) in 
print_bool (true = eval TRUE); 
print_bool (false = eval FALSE); 
print_bool (false = eval (NOT TRUE)); 
print_bool (true = eval (NOT FALSE)); 
print_bool (true = eval (ANDALSO (TRUE, TRUE))); 
print_bool (false = eval (ANDALSO (TRUE, FALSE))); 
print_bool (false = eval (ANDALSO (FALSE, TRUE))); 
print_bool (false = eval (ANDALSO (FALSE, FALSE))); 
print_bool (true = eval (ORELSE (TRUE, TRUE))); 
print_bool (true = eval (ORELSE (TRUE, FALSE))); 
print_bool (true = eval (ORELSE (FALSE, TRUE))); 
print_bool (false = eval (ORELSE (FALSE, FALSE))); 
print_bool (false = eval (IMPLY (TRUE, FALSE))); 
print_bool (true = eval (IMPLY (TRUE, TRUE))); 
print_bool (true = eval (IMPLY (FALSE, TRUE))); 
print_bool (true = eval (IMPLY (FALSE, FALSE))); 
print_bool (true = eval (LESS (NUM 3, NUM 5))); 
print_bool (false = eval (LESS (NUM 3, NUM 3))); 
print_bool (false = eval (LESS (NUM 3, NUM 1))); 
print_bool (false = eval 
(LESS (PLUS (NUM 3, NUM 4), MINUS (NUM 5, NUM 1)))); 
print_bool (true = eval 
(LESS (PLUS (NUM 10, NUM 12), MINUS (NUM 10, NUM (-13))))); 
