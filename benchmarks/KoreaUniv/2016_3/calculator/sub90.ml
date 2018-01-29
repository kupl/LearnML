
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp


  let rec exptoint : exp * exp -> int
  = fun (exp,integer) ->
  match exp with
  |X -> exptoint (integer, integer)
  |INT n -> n
  |ADD (ex1, ex2) -> exptoint (ex1, integer) + exptoint (ex2, integer)
  |SUB (ex1, ex2) -> exptoint (ex1, integer) - exptoint (ex2, integer)
  |MUL (ex1, ex2) -> exptoint (ex1, integer) * exptoint (ex2, integer)
  |DIV (ex1, ex2) -> exptoint (ex1, integer) / exptoint (ex2, integer)


  


  let rec calculator : exp -> int
  = fun exp -> (* TODO *)
  match exp with
  |X -> raise (NotImplemented)
  |INT n -> n
  |ADD (ex1, ex2) -> exptoint (ADD (ex1, ex2), INT 0)
  |SUB (ex1, ex2) -> exptoint (SUB (ex1, ex2), INT 0)
  |MUL (ex1, ex2) -> exptoint (MUL (ex1, ex2), INT 0)
  |DIV (ex1, ex2) -> exptoint (DIV (ex1, ex2), INT 0)
  |SIGMA (int1, int2, ex3) -> if exptoint (SUB (int1, int2), INT 0) <= 0 then exptoint (ex3, int1) + calculator (SIGMA(ADD (int1, INT 1),int2, ex3)) else 0
