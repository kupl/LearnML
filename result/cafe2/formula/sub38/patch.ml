type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec trans (e1 : exp) : exp =
  match e1 with
  | Plus (Num a, Num b) -> Num (a + b)
  | Plus (Num a, e3) -> Plus (Num a, trans e3)
  | Plus (e2, Num b) -> Plus (trans e2, Num b)
  | Plus (e2, e3) -> Plus (trans e2, trans e3)
  | Minus (Num a, Num b) -> Num (a - b)
  | Minus (Num a, e3) -> Minus (Num a, trans e3)
  | Minus (e2, Num b) -> Minus (trans e2, Num b)
  | Minus (e2, e3) -> Minus (trans e2, trans e3)
  | Num a -> Num a


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
  | Not f1 -> not (eval f1)
  | AndAlso (f1, f2) -> eval f1 && eval f2
  | OrElse (f1, f2) -> eval f1 || eval f2
  | Imply (f1, f2) -> (not (eval f1)) || eval f2
  | Equal (e1, e2) -> (
      match (__s3 e1, __s3 e2) with
      | Num __s14, Num __s15 -> __s14 = __s15
      | _ -> raise Failure "Exception(Template)" )