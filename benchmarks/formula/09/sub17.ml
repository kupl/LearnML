(* C:\Documents and Settings\Administrator\¹ÙÅÁ È­¸é\pl_second_homework\HW3_Exercise3.ml *)

(*Exercise 3*)
type expr = NUM of int
		| PLUS of expr*expr
		| MINUS of expr*expr

type formula = TRUE
	        | FALSE
	        | NOT of formula
		| ANDALSO of formula*formula
		| ORELSE of formula*formula
		| IMPLY of formula*formula
		| LESS of expr*expr
;;

let rec eval formul =
  let rec account expre =
	match expre with
	PLUS (x,y) -> (account x) + (account y)
	|MINUS (x,y) -> (account x) - (account y)	 	
	|NUM a -> a
	in	

	  match formul with	
          TRUE -> true
	  | FALSE -> false
	  | NOT x -> not (eval x)
	  | ANDALSO (x,y) -> (eval x) && (eval y)
	  | ORELSE (x,y) -> (eval x) || (eval y)
	  | IMPLY (x,y) -> (
				if ((eval x)=true && (eval y)=false) then false
				else true
			   )
	  | LESS (x,y) -> (
				if  (account x) < (account y) then true
				else false
			   )
	;;

