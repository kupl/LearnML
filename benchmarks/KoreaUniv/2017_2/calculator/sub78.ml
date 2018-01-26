(* problem 5 *)
type exp = X
        | INT of int
        | ADD of exp * exp
        | SUB of exp * exp
        | MUL of exp * exp
        | DIV of exp * exp
        | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> let rec func exp =
match exp with
  | X -> raise (Failure "fail")
  | INT num->num
  | ADD (e1, e2) -> func e1 + func e2
  | SUB (e1, e2) -> func e1 - func e2
  | MUL (e1, e2) -> func e1 * func e2
  | DIV (e1, e2) -> func e1 / func e2
  | SIGMA (e1, e2, e3) ->
   let a,b = func e1, func e2 in
    if a = b then evaluate e3 a 
    else if a < b then evaluate e3 a + func ( SIGMA (INT (a+1), INT b, e3))
    else raise (Failure "error")
  and  evaluate : exp -> int -> int
  = fun exp n ->
  match exp with
  | X -> n
  | INT n -> n
  | ADD (e1, e2) -> evaluate e1 n + evaluate e2 n
  | SUB (e1, e2) -> evaluate e1 n - evaluate e2 n
  | MUL (e1, e2) -> evaluate e1 n * evaluate e2 n
  | DIV (e1, e2) -> evaluate e1 n / evaluate e2 n
  | SIGMA _ -> func exp

in func e








