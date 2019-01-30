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
  let rec calculator_env exp env =
    match exp with
    | X -> env
    | INT x -> x
    | ADD (a, b) -> calculator_env a env + calculator_env b env
    | SUB (a, b) -> calculator_env a env - calculator_env b env
    | MUL (a, b) -> calculator_env a env * calculator_env b env
    | DIV (a, b) -> calculator_env a env / calculator_env b env
    | SIGMA (a, b, c) -> let v_stt = calculator_env a env
                          in let v_end = calculator_env b env
                            in sigma v_end c v_stt 0
and sigma v_end expr env accu =
  if env <= v_end then
    sigma v_end expr (env + 1) (accu + calculator_env expr env) (* SIGMA v_stt~v_end까지 더해줌*)
  else
    accu
in calculator_env exp 0;;

(*calculator (DIV(INT 3, INT 3));;*)
(*calculator (SIGMA(INT 1, INT 10, ADD(MUL(X, X), SIGMA(INT 2, INT 3, X))));;*)
(*calculator (SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1)))*)
