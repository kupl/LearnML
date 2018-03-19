
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec subsum : exp -> exp -> int
  = fun vals exps -> match exps with
    | INT k -> k
    | X -> subsum vals vals
    | ADD (exp1, exp2) -> (subsum vals exp1) + (subsum vals exp2)
    | SUB (exp1, exp2) -> (subsum vals exp1) - (subsum vals exp2)
    | MUL (exp1, exp2) -> (subsum vals exp1) * (subsum vals exp2)
    | DIV (exp1, exp2) -> (subsum vals exp1) / (subsum vals exp2)
    | SIGMA (expStart, expEnd, exp1) -> if(subsum vals expStart <= subsum vals expEnd)
      then (subsum expStart exp1) + (subsum vals (SIGMA(ADD(expStart, INT 1), expEnd, exp1))) else 0

  let rec calculator : exp -> int
  = fun exp -> match exp with
    | INT k -> k
    | ADD (exp1, exp2) -> (calculator exp1) + (calculator exp2)
    | SUB (exp1, exp2) -> (calculator exp1) - (calculator exp2)
    | MUL (exp1, exp2) -> (calculator exp1) * (calculator exp2)
    | DIV (exp1, exp2) -> (calculator exp1) / (calculator exp2)
    | SIGMA (expStart, expEnd, exp1) -> if(calculator expStart <= calculator expEnd)
      then (subsum expStart exp1) + (calculator (SIGMA(ADD(expStart, INT 1), expEnd, exp1))) else 0
    | _ -> 0
