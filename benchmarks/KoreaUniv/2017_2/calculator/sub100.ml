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
  |X -> 0
  |INT a -> a
  |ADD (a,b) -> calculator a + calculator b
  |SUB (a,b) -> calculator a - calculator b
  |MUL (a,b) -> calculator a * calculator b
  |DIV (a,b) -> if calculator b = 0
                then raise (Failure "0 problem")
                else calculator a / calculator b
  (*not completed*)
  |SIGMA (a,b,f) -> calculator a ;;