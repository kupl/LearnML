(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec apply : exp -> int -> exp
= fun e n ->
  match e with
  | INT n -> INT n
  | ADD (e1, e2) -> ADD ((apply e1 n), (apply e2 n))
  | SUB (e1, e2) -> SUB ((apply e1 n), (apply e2 n))
  | MUL (e1, e2) -> MUL ((apply e1 n), (apply e2 n))
  | DIV (e1, e2) -> DIV ((apply e1 n), (apply e2 n))
  | SIGMA (e1, e2, e3) -> SIGMA ((apply e1 n), (apply e2 n), e3)
  | X -> INT n

let rec calculator : exp -> int
= fun e -> 
  match e with
  | INT n -> n
  | ADD (e1, e2) -> (calculator e1) + (calculator e2)
  | SUB (e1, e2) -> (calculator e1) - (calculator e2)
  | MUL (e1, e2) -> (calculator e1) * (calculator e2)
  | DIV (e1, e2) -> 
    let v1 = (calculator e1) in
    let v2 =  (calculator e2) in
    if v2 = 0 then raise (Failure "Div by zero") else v1 / v2
  | SIGMA (e1, e2, e3) -> 
    let i = calculator e1 in
    let j = calculator e2 in 
    if (i > j) then raise (Failure "SIGMA Error") 
    else if (i = j) then (calculator (apply e3 i))
    else (calculator (apply e3 i)) + (calculator (SIGMA (ADD (e1, INT 1), e2, e3))) 
  | X -> raise (Failure "X is not bounded")