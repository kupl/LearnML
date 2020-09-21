type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp = 
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let rec expr : exp -> int = fun e ->
  match e with
    | Num (num) -> num
    | Plus (left, right) -> expr (left) + expr (right)
    | Minus (left, right) -> expr (left) - expr (right);;

let eval : formula -> bool
= fun f -> (*TODO*)
  let rec div f =
    match f with
      | True -> true
      | False -> false
      | Not (ev) -> if (div ev) then false else true
      | AndAlso (left, right) -> (div left) && (div right)
      | OrElse (left, right) -> (div left) || (div right)
      | Imply (left, right) -> (div (Not left)) || (div right)
      | Equal (left, right) -> (expr left) = (expr right) in div f;;
