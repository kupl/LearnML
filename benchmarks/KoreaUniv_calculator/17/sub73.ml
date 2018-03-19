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
  let rec ev_sigma : exp -> int -> int
  = fun e n ->
    match e with
    |X -> n
    |INT n -> n
    |ADD (a,b) -> ev_sigma a n + ev_sigma b n
    |SUB (a,b) -> ev_sigma a n - ev_sigma b n
    |MUL (a,b) -> ev_sigma a n * ev_sigma b n
    |DIV (a,b) -> ev_sigma a n / ev_sigma b n
    |SIGMA _ -> calculator e

in match e with
|X -> raise (Failure "Unbound value")
|INT n -> n
|ADD (a,b) -> calculator a + calculator b
|SUB (a,b) -> calculator a - calculator b
|MUL (a,b) -> calculator a * calculator b
|DIV (a,b) -> calculator a / calculator b
|SIGMA (a,b,c) ->
  if (calculator a) = (calculator b) then ev_sigma c (calculator a)
  else if (calculator a) < (calculator b) then ev_sigma c (calculator a) + calculator (SIGMA (INT ((calculator a) + 1), INT (calculator b), c))
  else raise (Failure "Error")
;;