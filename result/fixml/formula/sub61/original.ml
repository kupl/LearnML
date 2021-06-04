type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec evalexp exp =
  match exp with
  | Num n -> n
  | Plus (n1, n2) -> ( match (n1, n2) with _, _ -> evalexp n1 + evalexp n2 )
  | Minus (n1, n2) -> ( match (n1, n2) with _, _ -> evalexp n1 - evalexp n2 )


let rec eval : formula -> bool =
 fun f ->
  match f with
  | True -> true
  | False -> false
  | Not f' -> ( match f' with True -> false | False -> true | _ -> eval f' )
  | AndAlso (f1', f2') -> (
      match (f1', f2') with
      | True, True -> true
      | False, _ -> false
      | _, False -> false
      | _, _ -> eval f1' && eval f2' )
  | OrElse (f1', f2') -> (
      match (f1', f2') with
      | True, True -> true
      | True, False -> true
      | False, True -> true
      | False, False -> false
      | _, _ -> eval f1' || eval f2' )
  | Imply (f1', f2') -> (
      match (f1', f2') with
      | True, True -> true
      | True, False -> false
      | False, True -> true
      | False, False -> true
      | _, _ -> eval (Not f1') || eval f2' )
  | Equal (exp1, exp2) -> (
      match (evalexp exp1, evalexp exp2) with
      | a, b -> if a = b then true else false )
