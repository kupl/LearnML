exception Notavalue
exception InvalidSyntax

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
    match exp with
      X -> raise Notavalue
    | INT n -> n
    | (SIGMA (INT a, INT b, e)) -> 
        if a = b then evaluate e b 
                 else evaluate e a + calculator (SIGMA (INT (a+1), INT b, e))
    | _ -> evaluate exp 0
and evaluate e a =
    match e with
      X -> a
    | INT n -> n
    | ADD (e1, e2) -> (evaluate e1 a) + (evaluate e2 a)
    | SUB (e1, e2) -> (evaluate e1 a) - (evaluate e2 a)
    | MUL (e1, e2) -> (evaluate e1 a) * (evaluate e2 a)
    | DIV (e1, e2) -> (evaluate e1 a) / (evaluate e2 a)
    | SIGMA (INT k, INT n, e) -> calculator (SIGMA (INT k, INT n, e))
    | _ -> raise InvalidSyntax;;
    