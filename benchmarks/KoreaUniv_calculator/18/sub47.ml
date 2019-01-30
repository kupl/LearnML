type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calc_aux e n = 
  match e with
    |X -> n
    |INT a -> a
    |ADD (e1, e2) -> (calc_aux e1 n)+(calc_aux e2 n)
    |SUB (e1, e2) -> (calc_aux e1 n)-(calc_aux e2 n)
    |MUL (e1, e2) -> (calc_aux e1 n)*(calc_aux e2 n)
    |DIV (e1, e2) -> (calc_aux e1 n)/(calc_aux e2 n)
    |SIGMA(e1, e2, e3) ->
      let a = calc_aux e1 n in
      let b = calc_aux e2 n in
      if a > b then 0 else (calc_aux e3 a) + (calc_aux (SIGMA(INT(a+1), INT b, e3)) n);;


let rec calculator : exp -> int
= fun exp ->
match exp with
  |X -> raise(Failure "Incomputable Formula!!")
  |INT n -> n
  |ADD (n1, n2) -> calculator(n1) + calculator(n2)
  |SUB (n1, n2) -> calculator(n1) - calculator(n2)
  |MUL (n1, n2) -> calculator(n1) * calculator(n2)
  |DIV (n1, n2) -> calculator(n1) / calculator(n2)
  |SIGMA (n1, n2, n3) -> 
    let a = calculator(n1) in
    let b = calculator(n2) in
    if a > b then 0 else (calc_aux n3 a) + (calculator (SIGMA (INT(a+1), INT b, n3)));;
  