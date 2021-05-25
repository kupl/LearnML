type formula = True
				 | False
				 | Not of formula
				 | AndAlso of formula * formula
				 | OrElse of formula * formula
				 | Imply of formula * formula
				 | Equal of exp * exp
and exp = Num of int
		   | Plus of exp * exp
			| Minus of exp * exp

let rec eval3 (e:exp) : int =
	  match e with 
	  |Num n -> n
	  |Plus(e1, e2) -> eval3(e1) + eval3(e2)
	  |Minus(e1, e2) -> eval3(e1) - eval3(e2)

let rec eval2 (ff:formula) : formula =
	  match ff with 
	  |True -> True
	  |False -> False
	  |Not fn -> (  match eval2 fn with
					    |True -> False
						 |False -> True )
	  |AndAlso(f1,f2) -> ( match (eval2(f1),eval2(f2)) with
								  |(True,True) -> True
								  |(_,_) -> False )
	  |OrElse(f1,f2) -> ( match (eval2(f1),eval2(f2)) with
								  |(False,False) -> False
								  |(_,_) -> True )
	  |Imply(f1,f2) -> ( match (eval2(f1),eval2(f2)) with
								  |(True,False) -> False
								  |(_,_) -> True )
	  |Equal(e1,e2) -> if eval3(e1)=eval3(e2) then True
						   else False

let eval(f:formula) : bool = 
  match eval2(f) with
  |True -> true
  |False -> false
 
(*
let x1 : formula = AndAlso(True, False) (*False*)
let x2 : formula = OrElse(False, False) (*False*)
let x : formula = Imply(x1, x2)	 (*True*)
let x_value : bool = eval x

let _ = if x_value == true then  print_endline ("O")
		  else print_endline("X") (*O*)

let y1 : exp = Num 3
let y2 : exp = Num 4
let yp : exp = Plus(y1,y2)
let ym : exp = Minus(y2,y1)
let y : formula = Equal(yp,ym) (*False*)
let y_value : bool = eval y

let _ = if y_value == true then print_endline("O")
		  else print_endline("X") (*X*)
*)
(*--------------test case---------------------*)
(*
let _ = 
   let test_case : int * bool -> unit = fun (n, x) -> 
	     print_endline ("Case " ^ string_of_int(n) ^ " : " ^ string_of_bool(x)) in 
		     test_case(1, true = eval True); 
			     test_case(2, false = eval False); 
				     test_case(3, false = eval (Not True)); 
					     test_case(4, true = eval (Not False)); 
						     test_case(5, true = eval (AndAlso (True, True))); 
							     test_case(6, false = eval (AndAlso (True, False))); 
								     test_case(7, false = eval (AndAlso (False, True))); 
									     test_case(8, false = eval (AndAlso (False, False))); 
										     test_case(9, true = eval (OrElse (True, True))); 
											     test_case(10, true = eval (OrElse (True, False))); 
												     test_case(11, true = eval (OrElse (False, True))); 
													     test_case(12, false = eval (OrElse (False, False))); 
														     test_case(13, false = eval (Imply (True, False))); 
															     test_case(14, true = eval (Imply (True, True))); 
																     test_case(15, true = eval (Imply (False, True))); 
																	     test_case(16, true = eval (Imply (False, False))); 
																		     test_case(17, true = eval (Equal (Num 3, Num 5))); 
																			     test_case(18, false = eval (Equal (Num 3, Num 3))); 
																				     test_case(19, false = eval (Equal (Num 3, Num 1))); 
																					     test_case(20, false = eval (Equal (Plus (Num 3, Num 4), Minus (Num 5, Num 1)))); 
																						     test_case(21, true = eval (Equal (Plus (Num 10, Num 12), Minus (Num 10, Num (-13))))); 

*)
