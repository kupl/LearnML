(* School of Computer Science & Engineering
 * 2009-23151
 * Sungkeun Cho
 * HW 2 - Exercise 3
 *)


type formula = TRUE
	       | FALSE
	       | NOT of formula
	       | ANDALSO of formula * formula
	       | ORELSE of formula * formula
	       | IMPLY of formula * formula
	       | LESS of expr * expr
and expr = NUM of int
	   | PLUS of expr * expr
	   | MINUS of expr * expr;;

let rec eval f=
  let rec eval_e e=
    match e with
	NUM(i) -> i
      | PLUS(e1,e2) -> (eval_e e1) + (eval_e e2)
      | MINUS(e1,e2) -> (eval_e e1) - (eval_e e2)
  in
    match f with
	TRUE -> true
      | FALSE -> false
      | NOT(f1) -> not(eval f1)
      | ANDALSO(f1,f2) -> (eval f1) && (eval f2)
      | ORELSE(f1,f2) -> (eval f1) || (eval f2)
      | IMPLY(f1,f2) -> (not(eval f1)) || (eval f2)
      | LESS(e1,e2) -> (eval_e e1) < (eval_e e2)
;;

      
(*
eval(TRUE);;
eval(IMPLY(TRUE,ANDALSO(NOT(FALSE),TRUE)));;
eval(IMPLY(LESS(NUM(2),NUM(3))
	     ,ANDALSO(NOT(FALSE),TRUE)));;
eval(IMPLY(LESS(PLUS(NUM(1),NUM(2))
		  ,(MINUS(NUM(5),NUM(1))))
	     ,ANDALSO(NOT(FALSE),TRUE)));;
*)
