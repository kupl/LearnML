type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calc_x ex ei =
	match ex with
	| X -> ei
	| INT i -> i
	| ADD (a, b) -> calc_x a ei + calc_x b ei
	| SUB (a, b) -> calc_x a ei - calc_x b ei
	| MUL (a, b) -> calc_x a ei * calc_x b ei
	| DIV (a, b) -> calc_x a ei / calc_x b ei
	| _ -> 0

let rec calc_sum ei ej ex =
	if (ei = ej) then 
		calc_x ex ei
	else
		(calc_x ex ei + calc_sum (ei+1) ej ex)

let calculator : exp -> int
=fun e -> 
	match e with
	| SIGMA (ei, ej, ex) -> calc_sum (calc_x ei 0) (calc_x ej 0) ex
	| _ -> 0;;