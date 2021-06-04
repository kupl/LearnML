type exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

let rec exptoint (f : exp) : exp =
  match f with
  | Num n -> Num n
  | Plus (n1, n2) -> (
      match (n1, n2) with
      | Num e1, Num e2 -> Num (e1 + e2)
      | n1, n2 -> Plus (exptoint n1, exptoint n2) )
  | Minus (n1, n2) -> (
      match (n1, n2) with
      | Num e1, Num e2 -> Num (e1 - e2)
      | n1, n2 -> Minus (exptoint n1, exptoint n2) )


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not a -> not (eval a)
  | AndAlso (b1, b2) -> eval b1 && eval b2
  | OrElse (b1, b2) -> eval b1 || eval b2
  | Equal (e1, e2) -> if exptoint e1 = exptoint e2 then true else false
  | Imply (b1, b2) -> if eval b1 = true && eval b2 = false then false else true
