(* problem 5*)
type exp = X
        | INT of int
        | ADD of exp * exp
        | SUB of exp * exp
        | MUL of exp * exp
        | DIV of exp * exp
        | SIGMA of exp * exp * exp;;

let rec calculator : exp -> int
= fun e ->
  let rec eval : exp -> int -> int
  = fun e n -> match e with
                |INT n1 -> n1
                |X -> n
                |ADD (e1, e2) -> (eval e1 n) + (eval e2 n)
                |SUB (e1, e2) -> (eval e1 n) - (eval e2 n)
                |MUL (e1, e2) -> (eval e1 n) * (eval e2 n)
                |DIV (e1, e2) -> (eval e1 n) / (eval e2 n)
                |SIGMA (a, b, f) ->
                  if (eval a n) > (eval b n) then 0 else
                  (eval f (eval a n)) + eval (SIGMA (ADD (a, INT 1), b, f)) n
    in eval e 1;;