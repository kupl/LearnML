type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec calc (e : exp) : int =
  match e with
  | Num i -> i
  | Plus (ea, eb) -> calc ea + calc eb
  | Minus (ea, eb) -> calc ea - calc eb


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not fa -> not (eval fa)
  | AndAlso (fa, fb) -> ( match eval fa with true -> eval fb | _ -> false )
  | OrElse (fa, fb) -> ( match eval fa with true -> true | _ -> eval fb )
  | Imply (fa, fb) -> ( match eval fa with true -> eval fb | _ -> true )
  | Equal (ea, eb) -> calc ea = calc eb
