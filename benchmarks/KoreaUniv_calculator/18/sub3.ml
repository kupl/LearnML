type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec assign : exp -> exp -> exp = fun exp i ->
  match exp with
    | X -> i
    | INT n -> INT n
    | ADD (x, y) -> ADD (assign x i, assign y i)
    | SUB (x, y) -> SUB (assign x i, assign y i)
    | MUL (x, y) -> MUL (assign x i, assign y i)
    | DIV (x, y) -> DIV (assign x i, assign y i)
    | SIGMA (f, t, e) -> SIGMA (assign f i, assign t i, e);;

let rec calculator : exp -> int
= fun exp -> (*TODO*)
  match exp with
    | X -> raise (Failure "This case is unreachable")
    | INT n -> n
    | ADD (x, y) -> (calculator x) + (calculator y)
    | SUB (x, y) -> (calculator x) - (calculator y)
    | MUL (x, y) -> (calculator x) * (calculator y)
    | DIV (x, y) -> if ((calculator y) = 0) then raise (Failure "Div by zero error")
                    else (calculator x) / (calculator y)
    | SIGMA (f, t, e) -> if ((calculator f) > (calculator t)) then 0
                         else calculator (ADD (assign e f, SIGMA (ADD (f, INT 1), t, e)));;
