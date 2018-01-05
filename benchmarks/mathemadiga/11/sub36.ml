(* 2009-11824 Jieun-Jeong HW2-2 *)

type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

let rec mathemadiga exp =
	let rec calc e n =
		match e with
		|X		-> let substitute n =
						match n with 
						|Some i		-> i
						|None		-> raise (Invalid_argument "FreeVariable")
					in
					(substitute n)
		|INT num	-> (float_of_int num)
		|REAL num	-> num
		|ADD (el, er)	-> ((calc el n) +. (calc er n))
		|SUB (el, er)	-> ((calc el n) -. (calc er n))
		|MUL (el, er)	-> ((calc el n) *. (calc er n))
		|DIV (el, er)	-> ((calc el n) /. (calc er n))
		|SIGMA (start, finish, e)	-> 
							if (calc start n) >= (calc finish n)
								then (calc e (Some (calc finish n)))
								else ((calc e (Some (calc start n))) +. (calc (SIGMA ((REAL ((calc start n) +. 1.0)), finish, e)) n))
		|INTEGRAL (start, finish, e) ->
							if ((calc start n) -. (calc finish n)) > 0.01
								then (0.0 -. (calc (INTEGRAL (finish, start, e)) n))
							else if ((calc start n) -. (calc finish n)) < 0.01 && ((calc start n) -. (calc finish n)) > -0.01
								then 0.0 
								else (((calc e (Some (calc start n))) *. 0.1) +. (calc (INTEGRAL ((REAL ((calc start n) +. 0.1)), finish, e)) n))
		in
		(calc exp None)
