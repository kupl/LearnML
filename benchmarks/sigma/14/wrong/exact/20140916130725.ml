(*Exercise 1*)

let rec sigma a b f = 
  if a>b then 0
  else f a + sigma (a+1) b f


(*Exercise 2*)

(*given*)
type formula = TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
  and expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

(*solution*)
let rec eval f =
  let rec eval_expr e =
  match e with
  |NUM(a)->a
  |PLUS(a,b)->eval_expr(a)+eval_expr(b)
  |MINUS(a,b)->eval_expr(a)-eval_expr(b)
  in
  match f with
  |TRUE -> true
  |FALSE -> false
  |ANDALSO(a,b) -> (eval a) && (eval b)
  |ORELSE(a,b) -> (eval a) || (eval b)
  |NOT(a) -> not (eval a)
  |IMPLY(a,b) -> not (eval a) || (eval b)
  |LESS(a,b) -> eval_expr(a) < eval_expr(b);;



(*Exercise 3*)

(*given*)
type nat = ZERO|SUCC of nat

(*solution*)
let rec natadd a b = 
  match b with 
  |ZERO->a
  |SUCC(z)->SUCC(natadd a z)

let rec natmul a b =
  match b with
  |ZERO->ZERO
  |SUCC(z)->natadd a (natmul a z);;