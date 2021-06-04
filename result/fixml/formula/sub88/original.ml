type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval : formula -> bool =
 fun f ->
  let rec exptoint : exp -> int =
   fun e ->
    match e with
    | Num a -> a
    | Plus (a, b) -> exptoint a + exptoint b
    | Minus (a, b) -> exptoint a - exptoint b
  in

  match f with
  | True -> true
  | False -> false
  | Not x -> if x = True then false else true
  | AndAlso (x, y) -> eval x && eval y
  | OrElse (x, y) -> eval x || eval y
  | Imply (x, y) ->
      let imply : bool -> bool -> bool =
       fun a b -> if a = true && b = false then false else true
      in
      imply (eval x) (eval y)
  | Equal (x, y) -> if exptoint x = exptoint y then true else false
