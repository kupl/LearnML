(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e ->
  match e with
  | X -> raise (Failure "Variable X is not bounded")
  | INT n -> n
  | ADD (e1, e2) -> (calculator e1) + (calculator e2)
  | SUB (e1, e2) -> (calculator e1) - (calculator e2)
  | MUL (e1, e2) -> (calculator e1) * (calculator e2)
  | DIV (e1, e2) -> (calculator e1) / (calculator e2)
  | SIGMA (e1, e2, e3) ->
    let rec sigmacalc ex v =
      match ex with
      | X -> v
      | INT n -> n
      | ADD (e1, e2) -> (sigmacalc e1 v) + (sigmacalc e2 v)
      | SUB (e1, e2) -> (sigmacalc e1 v) - (sigmacalc e2 v)
      | MUL (e1, e2) -> (sigmacalc e1 v) * (sigmacalc e2 v)
      | DIV (e1, e2) -> (sigmacalc e1 v) / (sigmacalc e2 v)
      | SIGMA (e1, e2, e3) ->
          calculator (SIGMA (INT (sigmacalc e1 v), INT (sigmacalc e2 v), e3)) in
    let rec summation f l exp =
      if f > l then 0
      else (sigmacalc exp f) + (summation (f+1) l exp) in
    summation (calculator e1) (calculator e2) e3