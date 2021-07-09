type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval (f : formula) : bool =
  let rec exptoint (f1 : exp) : int =
    match f1 with
    | Num fs -> fs
    | Plus (fs, ls) -> exptoint fs + exptoint ls
    | Minus (fs, ls) -> exptoint fs - exptoint ls
  in

  match f with
  | True -> true
  | False -> false
  | AndAlso (fs, ls) -> eval fs && eval ls
  | OrElse (fst, lst) -> eval fst || eval lst
  | Imply (fst, lst) -> if eval fst && not (eval lst) then false else true
  | Equal (fs, ls) -> if exptoint fs = exptoint ls then true else false
  | Not a1 -> not (eval a1)
