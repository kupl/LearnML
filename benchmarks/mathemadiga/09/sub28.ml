(* School of Computer Science & Engineering
 * 2009-23151
 * Sungkeun Cho
 * HW 2 - Exercise 5
 *)

exception FreeVariable;;
exception InvalidSigma;;
exception DivideByZero;;

type exp = X
	   | INT of int
	   | REAL of float
	   | ADD of exp * exp
	   | SUB of exp * exp
	   | MUL of exp * exp
	   | DIV of exp * exp
	   | SIGMA of exp * exp * exp
	   | INTEGRAL of exp * exp * exp
;;


let rec mathemadiga e=
  let rec my_eval(e,x)=
    match e with
	X -> x
      | INT(i) -> Int32.to_float(Int32.of_int(i))
      | REAL(f) -> f
      | ADD(e1,e2) -> (my_eval (e1,x)) +. (my_eval (e2,x))
      | SUB(e1,e2) -> (my_eval (e1,x)) -. (my_eval (e2,x))
      | MUL(e1,e2) -> (my_eval (e1,x)) *. (my_eval (e2,x))
      | DIV(e1,e2) ->
	  let eval_e2 = my_eval (e2,x) in
	    if eval_e2 = 0.0
	    then raise DivideByZero
	    else (my_eval (e1,x)) /. eval_e2
      | SIGMA(e1,e2,e3) ->
	  let eval_e1 = my_eval (e1,x) in
	  let eval_e2 = my_eval (e2,x) in
	    if (eval_e1 > eval_e2) 
	    then raise InvalidSigma
	    else my_sigma(eval_e1,eval_e2,e3)
      | INTEGRAL(e1,e2,e3) ->
	  let eval_e1 = my_eval (e1,x) in
	  let eval_e2 = my_eval (e2,x) in
	    if (eval_e1 > eval_e2) 
	    then my_integral(eval_e2,eval_e1,e3) *. (-1.0)
	    else my_integral(eval_e1,eval_e2,e3)
  and my_sigma (f1,f2,e)=
    if f1 > f2 then 0.0
    else my_eval(e,f1) +. my_sigma(f1 +. 1.0,f2,e)
  and my_integral (f1,f2,e)=
    if f1 >= f2 then 0.0
    else if f2 -. f1 < 0.1 then my_eval(e,f1) *. (f2 -. f1)
    else my_eval(e,f1) *. 0.1 +. my_integral(f1 +. 0.1,f2,e)
  in
  match e with
      X -> raise FreeVariable
    | INT(i) -> Int32.to_float(Int32.of_int(i))
    | REAL(f) -> f
    | ADD(e1,e2) -> (mathemadiga e1) +. (mathemadiga e2)
    | SUB(e1,e2) -> (mathemadiga e1) -. (mathemadiga e2)
    | MUL(e1,e2) -> (mathemadiga e1) *. (mathemadiga e2)
    | DIV(e1,e2) ->
	let eval_e2 = mathemadiga e2 in
	  if eval_e2 = 0.0
	  then raise DivideByZero
	  else (mathemadiga e1) /. eval_e2
    | SIGMA(e1,e2,e3) ->
	let eval_e1 = mathemadiga e1 in
	let eval_e2 = mathemadiga e2 in
	  if (eval_e1 > eval_e2) 
	  then raise InvalidSigma
	  else my_sigma(eval_e1,eval_e2,e3)
    | INTEGRAL(e1,e2,e3) ->
	let eval_e1 = mathemadiga e1 in
	let eval_e2 = mathemadiga e2 in
	  if (eval_e1 > eval_e2) 
	  then my_integral(eval_e2,eval_e1,e3) *. (-1.0)
	  else my_integral(eval_e1,eval_e2,e3)
;;

(*
mathemadiga(X);;
mathemadiga(INT(3));;
mathemadiga(DIV(REAL(3.0),SUB(INT(5),INT(5))));;
mathemadiga(SIGMA ((REAL 1.5),(REAL 2.6),X));;
mathemadiga(INTEGRAL(REAL 1.0,REAL 10.0, X));;
mathemadiga(INTEGRAL(REAL 1.0,REAL 10.0, SUB(X,INT 1)));;
mathemadiga(
  INTEGRAL(
    SIGMA(INT(1),INT(2),X),
    SIGMA(INT(1),INT(3),X),
    SIGMA(X,ADD(X,INT(1)),
	  INTEGRAL(X,ADD(X,INT(1)),X))));;
*)
