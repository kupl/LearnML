(* 4190.310 Programming Language 			*
 * Homework #2 - Exercise 2 (mathemadiga)	*
 * 2008-11744 Jongwook Choi 				*)

type exp 	= X
			| INT of int
			| REAL of float
			| ADD of exp * exp
			| SUB of exp * exp
			| MUL of exp * exp
			| DIV of exp * exp
			| SIGMA of exp * exp * exp
			| INTEGRAL of exp * exp * exp

(* https://ropas.snu.ac.kr/phpbb/viewtopic.php?t=2235 *)
exception FreevarError
exception DividedByZero
exception InvalidSigmaError

let mathemadiga (e : exp) = 
	let rec aux (e : exp) (x : exp) =
		match e with
			  X -> (match x with
			  	  INT v -> (float_of_int v)
				| REAL v -> v
				| _ -> raise FreevarError )
			| INT v -> (float_of_int v)
			| REAL v -> v
			| ADD (l, r) -> (aux l x) +. (aux r x)
			| SUB (l, r) -> (aux l x) -. (aux r x)
			| MUL (l, r) -> (aux l x) *. (aux r x)
			| DIV (l, r) -> 
				let lhs = (aux l x) in 
				let rhs = (aux r x) in
				if rhs = 0. then raise DividedByZero
				else lhs /. rhs
			| SIGMA (f, t, b) ->
				(match (f, t) with (INT fval, INT tval) -> 
					let res = ref 0.0 in
					for i = fval to tval do
						res := !res +. (aux b (INT i))						
					done;
					!res
				| _ -> raise InvalidSigmaError)
			| INTEGRAL (f, t, b) ->
				let fval = (aux f x) in
				let tval = (aux t x) in
				let dx = 0.1 in
				let rec foraux x endx = 
					if x +. dx <= endx then 
						dx *. (aux b (REAL x)) +. (foraux (x+.dx) endx)
					else (endx -. x) *. (aux b (REAL x))
				in
				if tval = fval then 0.0
				else if fval > tval then 
					0.0 -. (foraux tval fval)
				else (foraux fval tval)
	in
		aux e X
;;

