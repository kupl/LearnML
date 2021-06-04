type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let andAlso (f : bool * bool) : bool =
  match f with true, true -> true | _ -> false


let orElse (f : bool * bool) : bool =
  match f with false, false -> false | _ -> true


let imply (f : bool * bool) : bool =
  match f with frue, false -> false | _ -> true


let rec ca (f : exp) : int =
  match f with
  | Num a -> a
  | Plus (a, b) -> ca a + ca b
  | Minus (a, b) -> ca a - ca b


let equal (f : exp * exp) : bool =
  match f with a, b -> if ca a = ca b then true else false


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | AndAlso (a, b) -> andAlso (eval a, eval b)
  | OrElse (a, b) -> orElse (eval a, eval b)
  | Imply (a, b) -> (not (eval a)) || eval b
  | Equal (a, b) -> equal (a, b)
  | Not __s5 -> not (eval __s5)
  | _ -> raise Failure "eval requires formula"
