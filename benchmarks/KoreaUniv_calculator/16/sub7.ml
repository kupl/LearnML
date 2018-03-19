
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

 let setx: (exp * int) -> int
  = fun (exp, int) ->
  match exp with
  | X -> int
  |_ -> raise (Failure ("no X to set value"));;

  let  rec calculate: (exp * int) -> int
  = fun (exp, int)  ->
  match exp with
  | X -> setx (exp, int)
  | INT n -> n
  | ADD (e1, e2) -> calculate (e1, int) + calculate (e2, int)
  | SUB (e1, e2) -> calculate (e1, int) - calculate (e2, int)
  | MUL (e1, e2) -> calculate (e1, int) * calculate (e2, int)
  | DIV (e1, e2) -> calculate (e1, int) / calculate (e2, int)
  | SIGMA (e1, e2, e3) -> if (calculate (e1, int) == calculate (e2, int)) 
                             then calculate (e3, calculate (e1,0)) 
                          else calculate (e3, calculate (e2,0)) +
                               calculate (SIGMA (e1, INT (calculate (e2,0) -1), e3), 0) ;;

  let calculator : exp -> int
  = fun exp -> calculate (exp, 0) ;;  (* TODO *)