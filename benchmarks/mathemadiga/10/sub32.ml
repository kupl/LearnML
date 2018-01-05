exception Error of string
exception FreevarError
exception DividedByZero

type exp = X
	     | INT of int
		 | REAL of float
		 | ADD of exp * exp
		 | SUB of exp * exp
		 | MUL of exp * exp
		 | DIV of exp * exp
		 | SIGMA of exp * exp * exp
		 | INTEGRAL of exp * exp * exp

let rec substitute (expr, var) =
 match expr with
 X -> var
 |(INT _) -> expr
 |(REAL _) -> expr
 |(ADD (e1, e2)) -> (ADD (substitute (e1, var),substitute (e2, var)))
 |(SUB (e1, e2)) -> (SUB (substitute (e1, var),substitute (e2, var)))
 |(MUL (e1, e2)) -> (MUL (substitute (e1, var),substitute (e2, var)))
 |(DIV (e1, e2)) -> (DIV (substitute (e1, var),substitute (e1, var)))
 |(SIGMA (e1, e2, e3)) -> (SIGMA (substitute (e1, var), substitute (e2, var), e3))
 |(INTEGRAL (e1, e2, e3)) -> (INTEGRAL (substitute (e1, var), substitute (e2, var), e3))

let is_int n = (n = (float_of_int (int_of_float n)))

let rec mathemadiga expr =
 match expr with
 X -> raise FreevarError
 |(INT n) -> (float_of_int n)
 |(REAL n) -> n
 |(ADD (e1, e2)) -> (mathemadiga e1) +. (mathemadiga e2)
 |(SUB (e1, e2)) -> (mathemadiga e1) -. (mathemadiga e2)
 |(MUL (e1, e2)) -> (mathemadiga e1) *. (mathemadiga e2)
 |(DIV (e1, e2)) -> (let eval1 = (mathemadiga e1) in
		     let eval2 = (mathemadiga e2) in
		     if (eval2) = 0.0 then raise DividedByZero
		     else eval1 /. eval2)
 |(SIGMA (e1, e2, e3)) -> (let first = (mathemadiga e1) in
		 	   let last = (mathemadiga e2) in
			   (if not ((is_int first) && (is_int last)) then raise (Error ("invalid index in sigma"))
			    else (let subs_expr = substitute (e3, REAL first) in
			          if (first > last) then raise (Error ("invalid index in sigma"))
				  else if (first = last) then (mathemadiga subs_expr)
				  else (mathemadiga subs_expr) +. (mathemadiga (SIGMA (REAL (first +. 1.0), e2, e3))))))
 |(INTEGRAL (e1, e2, e3)) -> (let first = (mathemadiga e1) in
		 	      let last = (mathemadiga e2) in
			      let subs_expr = (substitute (e3, REAL first)) in
			      (if (first > last) then -.(mathemadiga (INTEGRAL (e2, e1, e3)))
			       else if (first = last) then 0.0 (* because of top-left corner method *)
			       else (let delta = last -. first in
			             if delta >= 0.1 then
				     (((mathemadiga subs_expr) *. 0.1) +. (mathemadiga (INTEGRAL (REAL (first +. 0.1), e2, e3))))
				     else (* for make up integral range less then 0.1 *)
				     (((mathemadiga subs_expr) *. delta)))))


