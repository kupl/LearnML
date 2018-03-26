
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec sigma : int -> int -> exp -> int
  = fun start last exp ->
  if start > last then 0
  else (eval exp start) + (sigma (start + 1) last exp)

  and eval : exp -> int -> int
  = fun exp k ->
  match exp with
  | X -> k
  | INT i -> i
  | ADD (e1, e2) -> (eval e1 k) + (eval e2 k)
  | SUB (e1, e2) -> (eval e1 k) - (eval e2 k)
  | MUL (e1, e2) -> (eval e1 k) * (eval e2 k)
  | DIV (e1, e2) -> (eval e1 k) / (eval e2 k)
  | SIGMA (e1, e2, e3) ->
    let start = (eval e1 k) in
    let last = (eval e2 k) in
    (sigma start last e3)
		

  let calculator : exp -> int
  = fun exp -> 
  (eval exp 0)
