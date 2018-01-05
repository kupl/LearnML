type formula = TRUE
             | FALSE
             | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr

and  expr = NUM of int
             | PLUS of expr * expr
             | MINUS of expr * expr


let eval (fn:formula) :bool= match fn with
	TRUE -> true 
	|FALSE -> false 
	|NOT something -> (match something with 
					TRUE -> false 
					|FALSE-> true)
	|ANDALSO (TRUE,TRUE) -> true
	|ANDALSO (TRUE,FALSE) -> false
	|ANDALSO (FALSE,_) -> false

	|ORELSE (FALSE,FALSE)->false
	|ORELSE (FALSE,TRUE)->true
	|ORELSE (TRUE,_)->true

	|IMPLY (TRUE,FALSE)->false
	|IMPLY (TRUE,TRUE)->true
	|IMPLY (FALSE,_)->true

	|LESS (NUM n1,NUM n2)->n1<n2
	|LESS (PLUS (NUM n1,NUM n2),PLUS (NUM n3,NUM n4))->(n1+n2)<(n3+n4)
	|LESS (PLUS (NUM n1,NUM n2),MINUS (NUM n3,NUM n4))->(n1+n2)<(n3-n4)
	|LESS (MINUS (NUM n1,NUM n2),PLUS (NUM n3,NUM n4))->(n1-n2)<(n3+n4)
	|LESS (MINUS (NUM n1,NUM n2),MINUS (NUM n3,NUM n4))->(n1-n2)<(n3-n4)
