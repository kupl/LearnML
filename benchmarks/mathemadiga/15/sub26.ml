type exp = X
	 | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp

exception FreeVariable

let rec galculator: exp -> float = fun exp ->
        match exp with
        | X -> raise FreeVariable
        | INT x -> float_of_int x
        | REAL x -> x
        | ADD (e1,e2) -> galculator(e1) +. galculator(e2)
        | SUB (e1,e2) -> galculator(e1) -. galculator(e2)
        | MUL (e1,e2) -> galculator(e1) *. galculator(e2)
        | DIV (e1,e2) -> galculator(e1) /. galculator(e2)
        | SIGMA (e1,e2,e3) -> 
		let e1 = galculator(e1) in
		let e2 = galculator(e2) in
		if e1 > e2 then 0.
		else sum(1.0,e1,e2,e3)
        | INTEGRAL (e1, e2, e3) ->
		let e1 = galculator(e1) in
		let e2 = galculator(e2) in
                let gap = (e2 -. e1) in
                if (abs_float gap) < 0.1 then 0.
                else if gap > 0. then sum(0.1,e1,e2,e3)
                else -1. *. sum(0.1,e2,e1,e3)

and sum: ( float * float * float * exp) -> float = fun (interval,svalue,evalue,exp) ->
	let result = interval *. solvefun(svalue,exp) in
	if svalue +. interval <= evalue then
		result +. sum(interval,svalue+.interval,evalue,exp)
	else if interval = 1.0 then result
	else 0.

and solvefun: (float * exp) -> float = fun (svalue,exp) ->
        match exp with
        | X -> svalue
        | INT x -> float_of_int x
        | REAL x -> x
        | ADD (e1,e2) -> solvefun(svalue,e1) +. solvefun(svalue,e2)
        | SUB (e1,e2) -> solvefun(svalue,e1) -. solvefun(svalue,e2)
        | MUL (e1,e2) -> solvefun(svalue,e1) *. solvefun(svalue,e2)
        | DIV (e1,e2) -> solvefun(svalue,e1) /. solvefun(svalue,e2)
        | SIGMA (e1,e2,e3) -> 
		let e1 = galculator(e1) in
		let e2 = galculator(e2) in
		if e1 > e2 then 0.
		else sum(1.0,e1,e2,e3)
        | INTEGRAL (e1,e2,e3) ->
		let e1 = galculator(e1) in
		let e2 = galculator(e2) in
                let gap = e2 -. e1 in
                if abs_float gap < 0.1 then 0.
                else if gap > 0. then sum(0.1,e1,e2,e3)
                else -1. *. sum(0.1,e2,e1,e3)
