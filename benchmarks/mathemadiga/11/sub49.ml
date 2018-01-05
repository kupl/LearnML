type exp =
  | X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp

let mathemadiga e =
  let rec eval x e =
    match e with
    | X ->
      (match x with
      | None -> invalid_arg "eval : empty x"
      | Some v -> v)
    | INT i -> float_of_int i
    | REAL f -> f
    | ADD (e1,e2) -> eval x e1 +. eval x e2
    | SUB (e1,e2) -> eval x e1 -. eval x e2
    | MUL (e1,e2) -> eval x e1 *. eval x e2
    | DIV (e1,e2) -> eval x e1 /. eval x e2
    | SIGMA(bot,top,f) ->
      let l = int_of_float (eval x bot) in
      let u = int_of_float (eval x top) in
      if l<u then eval (Some (float_of_int l)) f +. eval x (SIGMA(INT (l+1), INT u, f))
      else if l=u then eval (Some (float_of_int l)) f
      else invalid_arg "eval : invalid range in sigma"
    | INTEGRAL(bot,top,f) ->
      let l = eval x bot in
      let u = eval x top in
      if l<u then eval (Some l) f *.0.1 +. eval x (INTEGRAL(REAL (l+.0.1), REAL u, f))
      else 0.0
  in
  eval None e
