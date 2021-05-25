type formula = True
            | False
            | Not of formula
            | AndAlso of formula * formula
            | OrElse of formula * formula
            | Imply of formula * formula  (* What is it? *)
            | Equal of exp * exp
            and exp = Num of int
            | Plus of exp * exp
            | Minus of exp * exp

let rec eval_exp ex =
	match ex with
	| Num a -> a
	| Plus (a,b) -> (eval_exp a) + (eval_exp b)
	| Minus (a,b) -> (eval_exp a) - (eval_exp b)


let rec eval form =
	match form with
	| True -> true
	| False -> false
	| Not form' -> not( eval form')
	| AndAlso (form1, form2 )-> (eval form1 ) && (eval form2)
	| OrElse (form1, form2)  -> (eval form1 ) || (eval form2)
	| Imply (form1, form2)   -> not (eval form1 )  || (eval form2) (* same as not ( eval form1 && not (eval form2)) *)
	| Equal (a,b) -> if( (eval_exp a) = (eval_exp b) ) then true else false

