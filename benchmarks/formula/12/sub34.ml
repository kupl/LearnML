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


let rec evalexpt expt = 
	match expt with
	  NUM a -> a
	| PLUS (x,y) -> (evalexpt x) + (evalexpt y)
	| MINUS (x,y) -> (evalexpt x) - (evalexpt y)



let rec eval formula = 
	match formula with
		  TRUE -> TRUE
		| FALSE -> FALSE
		| NOT a -> (if (eval a) = TRUE
						then FALSE
						else TRUE)
		| ANDALSO (a,b) -> (if (eval a) = TRUE && (eval b) = TRUE
								then TRUE
								else FALSE)
		| ORELSE (a,b) -> (if (eval a) = FALSE && (eval b) = FALSE
								then FALSE
								else TRUE)
		| IMPLY (a,b) -> (if (eval a) = TRUE && (eval b) = FALSE
								then FALSE
								else TRUE)
		| LESS (a,b) -> (if (evalexpt a) < (evalexpt b)
								then TRUE
								else FALSE)
