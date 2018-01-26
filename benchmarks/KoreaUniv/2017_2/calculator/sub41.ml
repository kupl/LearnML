(* problem 5 *)
type exp = X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e -> match e with
| X -> raise (Failure "string must be in SIGMA expression")
| INT n -> n
| ADD (e1, e2) -> calculator e1 + calculator e2
| SUB (e1, e2) -> calculator e1 - calculator e2
| MUL (e1, e2) -> calculator e1 * calculator e2
| DIV (e1, e2) -> calculator e1 / calculator e2
| SIGMA (e1, e2, e3) -> begin
  match e3 with
  | INT n -> (calculator e3) * 
             ((calculator e2 - calculator e1) + 1)
  | X -> (calculator e2) * (calculator e2 + 1 ) /2 -
         (calculator e1) * (calculator e1 - 1 ) /2
  | MUL (X, X) -> (calculator e2) *
                     ((calculator e2) * 2 + 1) *
                     ((calculator e2) + 1) / 6 -
                     (calculator e1) *
                     ((calculator e1) -1 ) *
                    ( ((calculator e1) *2) -1 ) /6
  | ADD (a, b) -> calculator (SIGMA (e1, e2, a)) +
                  calculator (SIGMA (e1, e2, b)) 
  | SUB (a, b) -> calculator (SIGMA (e1, e2, a)) -
                  calculator (SIGMA (e1, e2, b))
  end