type formula = TRUE
            | FALSE
            | NOT of formula
            | ANDALSO of formula * formula
            | ORELSE of formula * formula
            | IMPLY of formula * formula  (* What is it? *)
            | LESS of expr * expr
            and expr = NUM of int
            | PLUS of expr * expr
            | MINUS of expr * expr

let rec eval_expr ex =
	match ex with
	| NUM a -> a
	| PLUS (a,b) -> (eval_expr a) + (eval_expr b)
	| MINUS (a,b) -> (eval_expr a) - (eval_expr b)


let rec eval form =
	match form with
	| TRUE -> true
	| FALSE -> false
	| NOT form' -> not( eval form')
	| ANDALSO (form1, form2 )-> (eval form1 ) && (eval form2)
	| ORELSE (form1, form2)  -> (eval form1 ) || (eval form2)
	| IMPLY (form1, form2)   -> not (eval form1 )  || (eval form2) (* same as not ( eval form1 && not (eval form2)) *)
	| LESS (a,b) -> if( (eval_expr a) < (eval_expr b) ) then true else false

