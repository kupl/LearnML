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

let rec eval3 (e:expr) : int =
	  match e with 
	  |NUM n -> n
	  |PLUS(e1, e2) -> eval3(e1) + eval3(e2)
	  |MINUS(e1, e2) -> eval3(e1) - eval3(e2)

let rec eval2 (ff:formula) : formula =
	  match ff with 
	  |TRUE -> TRUE
	  |FALSE -> FALSE
	  |NOT fn -> (  match eval2 fn with
					    |TRUE -> FALSE
						 |FALSE -> TRUE )
	  |ANDALSO(f1,f2) -> ( match (eval2(f1),eval2(f2)) with
								  |(TRUE,TRUE) -> TRUE
								  |(_,_) -> FALSE )
	  |ORELSE(f1,f2) -> ( match (eval2(f1),eval2(f2)) with
								  |(FALSE,FALSE) -> FALSE
								  |(_,_) -> TRUE )
	  |IMPLY(f1,f2) -> ( match (eval2(f1),eval2(f2)) with
								  |(TRUE,FALSE) -> FALSE
								  |(_,_) -> TRUE )
	  |LESS(e1,e2) -> if eval3(e1)<eval3(e2) then TRUE
						   else FALSE

let eval(f:formula) : bool = 
  match eval2(f) with
  |TRUE -> true
  |FALSE -> false
 
(*
let x1 : formula = ANDALSO(TRUE, FALSE) (*FALSE*)
let x2 : formula = ORELSE(FALSE, FALSE) (*FALSE*)
let x : formula = IMPLY(x1, x2)	 (*TRUE*)
let x_value : bool = eval x

let _ = if x_value == true then  print_endline ("O")
		  else print_endline("X") (*O*)

let y1 : expr = NUM 3
let y2 : expr = NUM 4
let yp : expr = PLUS(y1,y2)
let ym : expr = MINUS(y2,y1)
let y : formula = LESS(yp,ym) (*FALSE*)
let y_value : bool = eval y

let _ = if y_value == true then print_endline("O")
		  else print_endline("X") (*X*)
*)
(*--------------test case---------------------*)
(*
let _ = 
   let test_case : int * bool -> unit = fun (n, x) -> 
	     print_endline ("Case " ^ string_of_int(n) ^ " : " ^ string_of_bool(x)) in 
		     test_case(1, true = eval TRUE); 
			     test_case(2, false = eval FALSE); 
				     test_case(3, false = eval (NOT TRUE)); 
					     test_case(4, true = eval (NOT FALSE)); 
						     test_case(5, true = eval (ANDALSO (TRUE, TRUE))); 
							     test_case(6, false = eval (ANDALSO (TRUE, FALSE))); 
								     test_case(7, false = eval (ANDALSO (FALSE, TRUE))); 
									     test_case(8, false = eval (ANDALSO (FALSE, FALSE))); 
										     test_case(9, true = eval (ORELSE (TRUE, TRUE))); 
											     test_case(10, true = eval (ORELSE (TRUE, FALSE))); 
												     test_case(11, true = eval (ORELSE (FALSE, TRUE))); 
													     test_case(12, false = eval (ORELSE (FALSE, FALSE))); 
														     test_case(13, false = eval (IMPLY (TRUE, FALSE))); 
															     test_case(14, true = eval (IMPLY (TRUE, TRUE))); 
																     test_case(15, true = eval (IMPLY (FALSE, TRUE))); 
																	     test_case(16, true = eval (IMPLY (FALSE, FALSE))); 
																		     test_case(17, true = eval (LESS (NUM 3, NUM 5))); 
																			     test_case(18, false = eval (LESS (NUM 3, NUM 3))); 
																				     test_case(19, false = eval (LESS (NUM 3, NUM 1))); 
																					     test_case(20, false = eval (LESS (PLUS (NUM 3, NUM 4), MINUS (NUM 5, NUM 1)))); 
																						     test_case(21, true = eval (LESS (PLUS (NUM 10, NUM 12), MINUS (NUM 10, NUM (-13))))); 

*)
