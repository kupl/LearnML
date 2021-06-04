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
   fun f1 ->
    match f1 with
    | Num fs -> fs
    | Plus (fs, ls) -> exptoint fs + exptoint ls
    | Minus (fs, ls) -> exptoint fs - exptoint ls
  in

  match f with
  | True -> true
  | False -> false
  | AndAlso (fs, ls) ->
      if fs = False then false else if ls = False then false else true
  | OrElse (fst, lst) ->
      if fst = True then true else if lst = True then true else false
  | Imply (fst, lst) -> if fst = True && lst = False then false else true
  | Equal (fs, ls) -> if exptoint fs = exptoint ls then true else false
  | Not a1 -> not (eval a1)
