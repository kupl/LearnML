
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let zero = INT 0
  let rec solveTerm : exp -> exp -> int
  = fun term k -> match term with
  | X -> solveTerm k k
  | INT n -> n
  | ADD (a, b) -> (solveTerm a k) + (solveTerm b k)
  | SUB (a, b) -> (solveTerm a k) - (solveTerm b k)
  | MUL (a, b) -> (solveTerm a k) * (solveTerm b k)
  | DIV (a, b) -> (solveTerm a k) / (solveTerm b k)
  | SIGMA (a, b, term) -> if ((solveTerm a k) > (solveTerm b k)) then 0
       else solveTerm (SIGMA ((INT(solveTerm a k + 1)), b, term)) k + solveTerm term a

  let rec calculator : exp -> int
  = fun exp -> solveTerm exp zero