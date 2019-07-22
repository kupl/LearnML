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

let rec cal(ex : expr) : int =
match ex with
|NUM i -> i
|PLUS (ex1, ex2) -> cal(ex1) + cal(ex2)
|MINUS (ex1, ex2) -> cal(ex1) - cal(ex2)

let rec eval (f : formula) : bool = 
match f with
|TRUE -> true
|FALSE -> false
|NOT f_not -> if eval(f_not) == true then false
  else true
|ANDALSO (f1,f2) -> eval(f1) && eval(f2)
|ORELSE (f1,f2) -> eval(f1) || eval(f2)
|IMPLY (f1,f2) -> eval(NOT f1) || eval(f2)
|LESS (ex1, ex2) -> if (cal(ex1) < cal(ex2)) then true
        else false 
