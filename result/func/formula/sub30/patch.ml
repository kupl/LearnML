type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec __s1 (__s2 : exp) : int =
  match __s2 with
  | Num __s14 -> __s14
  | Plus (__s15, __s16) -> __s1 __s15 + __s1 __s16
  | Minus (__s17, __s18) -> __s1 __s17 - __s1 __s18


let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not a -> not (eval a)
  | AndAlso (a, b) -> eval a && eval b
  | OrElse (a, b) -> eval a || eval b
  | Imply (a, b) -> if eval a then eval b else true
  | Equal (a, b) -> __s1 a = __s1 b


let rec expr (e : exp) : int =
  match e with
  | Num n -> n
  | Plus (e1, e2) -> expr e1 + expr e2
  | Minus (e1, e2) -> expr e1 - expr e2
