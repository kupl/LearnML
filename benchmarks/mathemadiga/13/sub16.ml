(* 2009-11718 2-1*)

(* 시그마 결과 0, 
 시그마 a,b 에 소수가 들어가면?*)
exception InvalidSigma
exception FreeVariable

type exp = X
		| INT of int
		| REAL of float
		| ADD of exp * exp
		| SUB of exp * exp
		| MUL of exp * exp
		| DIV of exp * exp
		| SIGMA of exp * exp * exp
		| INTEGRAL of exp * exp * exp

let rec cc exp =
		match exp with
		| X -> X
		| INT a -> (REAL (float_of_int a))
		| REAL a -> REAL a
		| ADD (ex1, ex2) -> ADD ((cc ex1), (cc ex2))
		| SUB (ex1, ex2) -> SUB ((cc ex1), (cc ex2))
		| MUL (ex1, ex2) -> MUL ((cc ex1), (cc ex2))
		| DIV (ex1, ex2) -> DIV ((cc ex1), (cc ex2))
		| SIGMA (a, b, ex) -> (sigma (INT (int_of_float (calc (cc a))), 
									  INT (int_of_float (calc (cc b))), ex))
		| INTEGRAL (a, b, ex) -> (integral ((cc a), (cc b), ex))

	and calc exp =
		match exp with
		| X -> raise FreeVariable
		| INT n -> (float_of_int n)
		| REAL n -> n
		| ADD (ex1, ex2) -> (calc ex1) +. (calc ex2)
		| SUB (ex1, ex2) -> (calc ex1) -. (calc ex2)
		| MUL (ex1, ex2) -> (calc ex1) *. (calc ex2)
		| DIV (ex1, ex2) -> (calc ex1) /. (calc ex2)
		| SIGMA (n, m, ex) -> (calc (cc (SIGMA (INT (int_of_float (calc (cc n))),
												INT (int_of_float (calc (cc m))),
												(cc ex)))))
		| INTEGRAL (n, m, ex) -> (calc (cc (INTEGRAL ((cc n), (cc m), (cc ex)))))

	and sigma (a, b, exp) =
		if (gal a) > (gal b) then (REAL 0.)
		else if (gal a) = (gal b) then REAL (calc (sCalc ((gal a), exp)))
		else REAL (calc (ADD ((sigma (INT (int_of_float (calc (ADD ((cc a), INT 1)))), (cc b), exp)),
							  (REAL (calc (sCalc ((gal a), exp)))))))

		(*sigma calculator*)
	and sCalc (a, exp) = 
		match exp with
		| X -> REAL a
		| INT n -> REAL (float_of_int n)
		| REAL n -> REAL n
		| ADD (ex1, ex2) -> REAL (calc (ADD ((sCalc (a, ex1)), (sCalc (a, ex2)))))
		| SUB (ex1, ex2) -> REAL (calc (SUB ((sCalc (a, ex1)), (sCalc (a, ex2)))))
		| MUL (ex1, ex2) -> REAL (calc (MUL ((sCalc (a, ex1)), (sCalc (a, ex2)))))
		| DIV (ex1, ex2) -> REAL (calc (DIV ((sCalc (a, ex1)), (sCalc (a, ex2)))))
		| SIGMA (n, m, ex) -> (sigma ((INT (int_of_float (calc (sCalc (a, (cc n)))))),
									  (INT (int_of_float (calc (sCalc (a, (cc m)))))),
									  ex))
		| INTEGRAL (n, m, ex) -> (integral ((REAL (calc (sCalc (a, (cc n))))),
											(REAL (calc (sCalc (a, (cc m))))),
											ex))

	and integral (a, b, exp) =
		if (gal a) = (gal b) then (REAL 0.)
		else if ((gal a) > (gal b)) then MUL ((integral (b, a, exp)), (REAL (-1.)))
		else if ((gal a) < (gal (SUB (b, REAL 0.1))))
			then REAL (calc (ADD (REAL (calc (MUL ((REAL (calc (iCalc ((gal a), exp)))), REAL 0.1))),
								 (integral (REAL (calc (ADD ((cc a), REAL 0.1))), (cc b), exp)))))
		else MUL ((REAL (calc (iCalc ((gal a), exp)))),
				  SUB ((cc b), (cc a)))

		(*integral calculator*)
	and iCalc (a, exp) =
		match exp with
		| X -> REAL a
		| INT n -> REAL (float_of_int n)
		| REAL n -> REAL n
		| ADD (ex1, ex2) -> (REAL (calc (ADD ((iCalc (a, ex1)), (iCalc (a, ex2))))))
		| SUB (ex1, ex2) -> (REAL (calc (SUB ((iCalc (a, ex1)), (iCalc (a, ex2))))))
		| MUL (ex1, ex2) -> (REAL (calc (MUL ((iCalc (a, ex1)), (iCalc (a, ex2))))))
		| DIV (ex1, ex2) -> (REAL (calc (DIV ((iCalc (a, ex1)), (iCalc (a, ex2))))))
		| SIGMA (n, m, ex) -> (sigma ((INT (int_of_float (calc (iCalc (a, (cc n)))))),
									  (INT (int_of_float (calc (iCalc (a, (cc m)))))),
									  ex))
		| INTEGRAL (n, m, ex) -> (integral ((REAL (calc (iCalc (a, (cc n))))),
											(REAL (calc (iCalc (a, (cc m))))),
											ex))

	and gal exp =
		(calc (cc exp))

let galculator exp =
	(gal exp)


