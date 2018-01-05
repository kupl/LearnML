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

let rec galculator (e:exp) :float =
  let rec eVal (org:exp) (value:float) :exp =
    match org with
    | X -> REAL value
    | ADD (e1, e2) -> ADD (eVal e1 value, eVal e2 value)  
    | SUB (e1, e2) -> SUB (eVal e1 value, eVal e2 value)
    | MUL (e1, e2) -> MUL (eVal e1 value, eVal e2 value)
    | DIV (e1, e2) -> DIV (eVal e1 value, eVal e2 value)
    | SIGMA (e1, e2, e3) -> SIGMA (eVal e1 value, eVal e2 value, eVal e3 value)
	| INTEGRAL (e1, e2, e3) -> INTEGRAL (eVal e1 value, eVal e2 value, eVal e3 value)
	| _ as t -> t
  in
  match e with
  | X -> raise FreeVariable
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD (e1, e2) -> (galculator e1) +. (galculator e2)
  | SUB (e1, e2) -> (galculator e1) -. (galculator e2)
  | MUL (e1, e2) -> (galculator e1) *. (galculator e2)
  | DIV (e1, e2) -> (galculator e1) /. (galculator e2)
  | SIGMA (down, up, ex) -> 
    let rec sigma (bot:int) (top:int) (e:exp) :float =
	  let dif = top - bot in
	  if dif < 0 then 0.0
	  else (sigma (bot + 1) top e) +. (galculator (eVal e (float_of_int bot)))
    in sigma (int_of_float (galculator down)) (int_of_float (galculator up)) ex
  | INTEGRAL (down, up, ex) -> 
    let rec integral (bot:float) (top:float) (e:exp) :float =
	  let dif = top -. bot in
	  if dif < 0.1 then 0.0
	  else (integral (bot +. 0.1) top e) +. (galculator (eVal e bot))
    in 
	let ui = (galculator up) and mit = (galculator down)
	in
    if ui < mit then -1.0 *. ((integral ui mit ex) *. 0.1) else (integral mit ui ex) *. 0.1
	
