exception NotImplemented

  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec sigma n exp =
match exp with
|INT i -> i
|X -> n
|ADD (e1,e2) -> (sigma n e1) + (sigma n e2)
|MUL (e1,e2) -> (sigma n e1) * (sigma n e2)
|DIV (e1,e2) -> (sigma n e1) / (sigma n e2)
|SUB (e1,e2) -> (sigma n e1) - (sigma n e2)
|_ -> raise NotImplemented

let rec calculator : exp -> int
  = fun exp ->
match exp with
|X -> raise NotImplemented
|INT n -> n
|ADD (e1, e2) -> (calculator e1) + (calculator e2)
|SUB (e1, e2) -> (calculator e1) - (calculator e2)
|MUL (e1, e2) -> (calculator e1)*(calculator e2)
|DIV (e1, e2) -> (calculator e1)/(calculator e2)
|SIGMA (e1, e2, e3) ->
if ((calculator e1) > (calculator e2)) then raise NotImplemented
else if ((calculator e1) == (calculator e2)) then sigma (calculator e2) e3
else sigma (calculator e1) e3 + calculator (SIGMA ((ADD(e1, INT 1), e2, e3)))
