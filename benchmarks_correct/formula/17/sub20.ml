type formula = True
              | False
              | Not of formula
              | AndAlso of formula * formula
              | OrElse of formula * formula
              | Imply of formula * formula
              | Equal of exp * exp
              and exp = Num of int
              | Plus of exp * exp
              | Minus of exp * exp

let rec eval (f: formula) =
  let rec calc (e: exp) =
    match e with
    | Num n -> n
    | Plus (a, b) -> (calc a) + (calc b)
    | Minus (a, b) -> (calc a) - (calc b)
  in
  match f with
    | True -> true
    | False -> false
    | Not f' -> not (eval f')
    | AndAlso (f', f'') -> (eval f') && (eval f'')
    | OrElse (f', f'') -> (eval f') || (eval f'')
    | Imply (f', f'') -> not ((eval f') && (not (eval f'')))
    | Equal (e, e') -> (calc e) = (calc e')
    | _ -> false