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

let rec eval = fun formula -> let rec temp expr = match expr with
						NUM a -> a
						|PLUS (a,b) -> (temp a) + (temp b)
						|MINUS (a,b) -> (temp a) - (temp b) in 
				
				 match formula with 
  				|TRUE -> true
  				|FALSE -> false
  				|NOT f1 -> if (eval f1) = true then false else true
  				|ANDALSO (f1, f2) -> (eval f1) & (eval f2)
  				|ORELSE (f1,f2) -> (eval f1) || (eval f2)
  				|IMPLY (f1, f2) -> ((eval f1) & (eval f2)) || (eval (NOT f1))
  				|LESS (e1, e2) -> if (temp e1) < (temp e2) then true else false;;

let a = TRUE;;
let b = FALSE;;
let c = NOT TRUE;;
let d = PLUS (NUM 1, NUM 2);;
let e = MINUS(NUM 4, NUM 3);;
