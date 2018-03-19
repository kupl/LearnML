(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let temp_x = ref (-12541);;
let rec calculator : exp -> int
= fun e ->
	match e with
	| X -> if !temp_x = (-12541) then raise (Failure "Non-initialized variable error") else !temp_x
	| INT i -> i
	| ADD (e1, e2) -> (calculator e1) + (calculator e2)
	| SUB (e1, e2) -> (calculator e1) - (calculator e2)
	| MUL (e1, e2) -> (calculator e1) * (calculator e2)
	| DIV (e1, e2) -> (
		let cal_e2 = calculator e2 in
		match cal_e2 with
		| 0 -> raise (Failure "divide by zero error")
		| _ -> (calculator e1) / cal_e2
	)
	| SIGMA (e1, e2, e3) -> (		
		let old_temp_x = !temp_x in
		let cal_e1 = calculator e1 in
		let cal_e2 = calculator e2 in
		temp_x := cal_e1;
		let cal_e3 = calculator e3 in
		temp_x := old_temp_x;
		if cal_e1 > cal_e2 then (raise (Failure "e2 larger than e1 error"))
		else if cal_e1 == cal_e2 then cal_e3
		else cal_e3 + (calculator (SIGMA (INT (cal_e1 + 1), e2, e3)))
	)
;;