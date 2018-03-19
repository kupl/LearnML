(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e ->
  let rec eval = fun e x ->
    match e with
    | X -> x
    | INT n -> n
    | ADD (a, b) -> (eval a x) + (eval b x)
    | SUB (a, b) -> (eval a x) - (eval b x)
    | MUL (a, b) -> (eval a x) * (eval b x)
    | DIV (a, b) -> (eval a x) / (eval b x)
    | SIGMA (start', end', body) ->
      let rec loop = fun i e ->
        if i <= e then
          (eval body i) + loop (i + 1) e
        else
          0
      in
      loop (eval start' x) (eval end' x)
  in
  eval e 0