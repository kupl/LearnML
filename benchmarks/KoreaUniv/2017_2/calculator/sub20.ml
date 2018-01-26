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
  let rec placement : exp -> exp -> exp
  = fun e a ->
    match e with
    | X -> a
    | INT(x) -> INT(x)
    | ADD(x, y) -> ADD(placement x a, placement y a)
    | SUB(x, y) -> SUB(placement x a, placement y a)
    | MUL(x, y) -> MUL(placement x a, placement y a)
    | DIV(x, y) -> DIV(placement x a, placement y a)
    | SIGMA(a, b, c) -> INT(calculator(SIGMA(a, b, c)))
  in
  match e with
  | X -> 0
  | INT(a) -> a
  | ADD(a, b) -> calculator(a) + calculator(b)
  | SUB(a, b) -> calculator(a) - calculator(b)
  | MUL(a, b) -> calculator(a) * calculator(b)
  | DIV(a, b) -> calculator(a) / calculator(b)
  | SIGMA(a, b, c) ->
      let rec sigma
      = fun a b c ->
        if a = b then calculator (placement c a)
        else
          (calculator(placement c a)) + (sigma (INT(calculator(ADD(a, INT(1))))) b c)
      in
      sigma a b c
;;
