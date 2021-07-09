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


let rec __s3 (__s4 : exp) : exp =
  match __s4 with
  | Num __s16 -> Num __s16
  | Plus (Num __s17, Num __s18) -> Num (__s17 + __s18)
  | Plus (__s19, __s20) -> __s3 (Plus (__s3 __s19, __s3 __s20))
  | Minus (Num __s21, Num __s22) -> Num (__s21 - __s22)
  | Minus (__s23, __s24) -> __s3 (Minus (__s3 __s23, __s3 __s24))


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not a -> not (eval a)
  | AndAlso (b1, b2) -> eval b1 && eval b2
  | OrElse (b1, b2) -> eval b1 || eval b2
  | Equal (e1, e2) -> (
      match (__s3 e1, __s3 e2) with
      | Num __s14, Num __s15 -> __s14 = __s15
      | _ -> raise Failure "Exception(Template)" )
  | Imply (b1, b2) -> (not (eval b1)) || eval b2