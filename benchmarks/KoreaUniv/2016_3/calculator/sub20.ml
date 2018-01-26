
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec cal_sigma : exp -> int
  = fun exp -> 0
  
  let rec cal_exp : int -> exp -> int
  = fun x exp -> match exp with
  | X -> x 
  | INT n -> n
  | ADD (e1,e2) -> (cal_exp x e1) + (cal_exp x e2)
  | SUB (e1,e2) -> (cal_exp x e1) - (cal_exp x e2)
  | MUL (e1,e2) -> (cal_exp x e1) * (cal_exp x e2)
  | DIV (e1,e2) -> (cal_exp x e1) / (cal_exp x e2)
  | SIGMA(e1,e2,e3) -> (match e1,e2,e3 with
	| INT v1, INT v2, v3 -> if v1>v2 then 0
	else (cal_exp v2 v3) + cal_exp 0 (SIGMA(INT v1,INT (v2-1),v3))
	| v1,v2,v3 -> 
	let n1 = cal_exp 0 v1 in
	let n2 = cal_exp 0 v2 in
	if n1>n2 then 0
	else (cal_exp n2 v3) + cal_exp 0 (SIGMA(INT n1,INT (n2-1),v3))
	)
    
  let calculator : exp -> int
  = fun exp -> cal_exp 0 exp
