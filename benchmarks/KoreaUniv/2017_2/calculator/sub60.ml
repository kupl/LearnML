(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e -> (* TODO *)
  match e with
    | X -> 0
    | INT integer -> integer
    | ADD (exp1, exp2) -> (calculator exp1 + calculator exp2)
    | SUB (exp1, exp2) -> (calculator exp1 - calculator exp2)
    | MUL (exp1, exp2) -> (calculator exp1 * calculator exp2)
    | DIV (exp1, exp2) -> 
      if (calculator exp2 = 0) then 0
      else (calculator exp1 / calculator exp2)
    | SIGMA (exp1, exp2, exp3) ->
        let rec cal : int * exp -> int
        = fun (index, form) -> 
          match form with
          | X -> index
          | INT integer -> integer
          | ADD (exp1, exp2) -> (cal (index, exp1) + cal (index, exp2))
          | SUB (exp1, exp2) -> (cal (index, exp1) - cal (index, exp2))
          | MUL (exp1, exp2) -> (cal (index, exp1) * cal (index, exp2))
          | DIV (exp1, exp2) -> (cal (index, exp1) / cal (index, exp2))
          | SIGMA (exp1, exp2, exp3) -> calculator (SIGMA (exp1, exp2, exp3)) in
      if (calculator exp1 > calculator exp2) then 0 
      else (cal (calculator exp1, exp3) + calculator(SIGMA (ADD (exp1, INT 1), exp2, exp3)))