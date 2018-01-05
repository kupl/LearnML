(*2009-11718 1-5*)
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

let rec eval fml =
	let rec exp exp1 = 
	match exp1 with
	NUM a -> a
	| PLUS (a,b) -> (exp a) + (exp b)
	| MINUS (a,b) -> (exp a) - (exp b) in

	match fml with
	TRUE -> true
	| FALSE -> false
	| NOT a -> not (eval a)
	| ANDALSO (a,b) -> (eval a)&&(eval b)
	| ORELSE (a,b) -> (eval a)||(eval b)
	| IMPLY (a,b) -> (if (eval a)=true&&(eval b)=false then
			false
			else true)
	| LESS (a,b) -> (if (exp a) < (exp b) then
			true
			else false)
		
(* LESS ¶æ?*)				
