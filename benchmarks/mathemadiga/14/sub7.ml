type exp = X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp;;

exception FreeVariable;;


(*
let rec print_exp exp =
  match exp with
    X -> "x"
    | INT i -> string_of_int i
    | REAL f -> string_of_float f
    | ADD(e1, e2) -> String.concat " + " [print_exp e1;print_exp e2]
    | SUB(e1, e2) -> String.concat " - " [print_exp e1;print_exp e2]
    | MUL(e1, e2) -> String.concat " * " [print_exp e1;print_exp e2]
    | DIV(e1, e2) -> String.concat " / " [print_exp e1;print_exp e2]
    | SIGMA(e1, e2, e3) -> String.concat "" ["SIGMA _";print_exp e1;" ^";print_exp e2;" ";print_exp e3]

let print_float f = 
  print_endline (Printf.sprintf "%.5f" f);;
*)

type variable = UNDEFINED | VALUE of float

let galculator exp =
  let rec galWithVar (exp, variable) = 
    (* custom type : variable *)
    (* float 인 것으로만 가정하자 *)
    match exp with
      X -> (
        match variable with
          UNDEFINED -> raise FreeVariable
          | VALUE (x) -> x)
      | INT i -> float_of_int i
      | REAL f -> f
      | ADD(e1, e2) -> (galWithVar (e1, variable) +. galWithVar (e2, variable))
      | SUB(e1, e2) -> (galWithVar (e1, variable) -. galWithVar (e2, variable))
      | MUL(e1, e2) -> (galWithVar (e1, variable) *. galWithVar (e2, variable))
      | DIV(e1, e2) -> (galWithVar (e1, variable) /. galWithVar (e2, variable))
      | SIGMA(e1, e2, e3) ->
        let value_of_e1 = galWithVar(e1, variable) in
        let value_of_e2 = galWithVar(e2, variable) in
          if (value_of_e1 > value_of_e2)
            then 0.0
          else
            (* e1 을 e3 에 대입시키고 그 결과와 나머지를 더합니다 *)
            (galWithVar (e3, VALUE (value_of_e1))) +.
            (galWithVar (SIGMA(INT (int_of_float (value_of_e1) + 1), e2, e3), variable))
      | INTEGRAL(e1, e2, e3) ->
        let value_of_e1 = galWithVar(e1, variable) in
        let value_of_e2 = galWithVar(e2, variable) in
        if value_of_e1 > value_of_e2
          then -1.0 *. galWithVar (INTEGRAL(e2, e1, e3), variable)
        else if value_of_e2 -. value_of_e1 < 0.1
          then 0.0
        else
          ((galWithVar (e3, VALUE (value_of_e1))) *. 0.1) +.
          (galWithVar (INTEGRAL(REAL (value_of_e1 +. 0.1), e2, e3), variable)) in
  galWithVar (exp, UNDEFINED);;

(*
print_endline (Printf.sprintf "%.5f" (galculator (INTEGRAL(REAL 1.0, REAL 10.0, SUB(MUL(X, X), INT 1)))));;
*)

(*
print_float (galculator (INTEGRAL(INT 0, INT 10000, DIV (SUB(MUL(INT 16, DIV(X, INT 10000)), INT 16),ADD(SUB(MUL(MUL(DIV(X, INT 10000),DIV(X, INT 10000)),MUL(DIV(X, INT 10000),DIV(X, INT 10000))), MUL(MUL(INT 2,DIV(X, INT 10000)),MUL(DIV(X, INT 10000),DIV(X, INT 10000)))) , SUB(MUL(INT 4, DIV(X, INT 10000)), INT 4) ) ) )));; 
*)
(*
print_float (galculator(SIGMA(INT 1, INT 1000, DIV(INT 8, MUL(SUB(MUL(INT 2, X), INT 1),SUB(MUL(INT 2, X), INT 1))))));;
*)
