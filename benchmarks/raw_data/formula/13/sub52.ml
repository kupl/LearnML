(* syntax *)
type formula = True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp
  and exp = Num of int
  | Plus of exp * exp
  | Minus of exp * exp ;;

(* semantics *)
let rec eval (formula) =
  let rec eval_exp (exp) =
    match exp with
    | Num n -> n
    | Plus (a, b) -> eval_exp(a) + eval_exp(b)
    | Minus (a, b) -> eval_exp(a) - eval_exp(b) in
  match formula with
  | True -> true
  | False -> false
  | Not a -> not(eval(a))
  | AndAlso (a, b) -> eval(a) && eval(b)
  | OrElse (a, b) -> eval(a) || eval(b)
  | Imply (a, b) -> not(eval(a) && not(eval(b)))
  | Equal (a, b) -> eval_exp(a) = eval_exp(b) ;;

(* test cases *)


