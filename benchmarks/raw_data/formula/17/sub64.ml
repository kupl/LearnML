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

let rec exp2int (n: exp) : int =
  match n with
  | Num x -> x
  | Plus (x, y) -> (exp2int x) + (exp2int y)
  | Minus (x, y) -> (exp2int x) - (exp2int y)

let rec eval (f: formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not f' -> (match (eval f') with
              | true -> false
              | false -> true)
  | AndAlso (a, b) -> if ((eval a) == false) then false
                      else if ((eval b) == false) then false
                      else true
  | OrElse (a, b) -> if ((eval a) == true) then true
                     else if ((eval b) == true) then true
                     else false
  | Imply (a, b) -> if ((eval a) == false) then true
                    else if ((eval b == true)) then true
                    else false
  | Equal (x, y) -> if (exp2int x) = (exp2int y) then true
                   else false
