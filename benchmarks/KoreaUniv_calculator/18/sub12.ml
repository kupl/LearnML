type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp ->
  let rec calc_sigma exp i =
    match exp with
      | X -> i
      | INT n -> n
      | ADD (e1, e2) -> (calc_sigma e1 i) + (calc_sigma e2 i)
      | SUB (e1, e2) -> (calc_sigma e1 i) - (calc_sigma e2 i)
      | MUL (e1, e2) -> (calc_sigma e1 i) * (calc_sigma e2 i)
      | DIV (e1, e2) -> (calc_sigma e1 i) / (calc_sigma e2 i) 
      | SIGMA (exp1,exp2,exp3) -> 
        if (calc_sigma exp1 0)<=(calc_sigma exp2 0) 
        then (calc_sigma exp3 (calc_sigma exp1 0))+(calc_sigma (SIGMA (INT ((calc_sigma exp1 0)+1), exp2, exp3)) 0)
        else 0
        in
  calc_sigma exp 0;;
  