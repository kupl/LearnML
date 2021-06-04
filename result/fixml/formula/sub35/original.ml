type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec cal : exp -> int =
 fun e ->
  match e with
  | Num n -> n
  | Plus (e1, e2) -> cal e1 + cal e2
  | Minus (e1, e2) -> cal e1 - cal e2


let rec eval : formula -> bool =
 fun f ->
  match f with
  | True -> true
  | False -> false
  | Not f1 -> not (eval f1)
  | AndAlso (f1, f2) -> eval f1 && eval f2
  | OrElse (f1, f2) -> eval f1 || eval f2
  | Imply (f1, f2) -> eval (Not f1) && eval f2
  | Equal (e1, e2) ->
      let rec cal : exp -> int =
       fun e ->
        match e with
        | Num n -> n
        | Plus (x, y) -> cal x + cal y
        | Minus (x, y) -> cal x - cal y
      in
      cal e1 = cal e2
