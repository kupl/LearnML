type formula = True | False | Not of formula | AndAlso of formula * formula
             | OrElse of formula * formula | Imply of formula * formula
             | Equal of exp * exp
and exp = Num of int | Plus of exp * exp | Minus of exp * exp

let rec exp_to_int (x: exp): int = 
  match x with
  | Num x -> x
  | Plus (a, b) -> (exp_to_int a)+(exp_to_int b)
  | Minus (a, b) -> (exp_to_int a)-(exp_to_int b)

let eval_less ((x: int), (y: int)): bool = x = y

let rec bool_to_formula (b : bool): formula =
  match b with
  | true -> True
  | false -> False

let rec eval (f : formula): bool = 
  match f with
  | True -> true
  | False -> false
  | Not a -> (match a with
              | True -> false
              | False -> true
              | a -> eval (Not (bool_to_formula (eval a)))
             )
  | AndAlso (a, b) -> (match (a, b) with
                      | (True, True) -> true
                      | (True, False) -> false
                      | (False, True) -> false
                      | (False, False) -> false
                      | (a, b) -> eval (AndAlso (bool_to_formula (eval a), bool_to_formula (eval b)))
                      )
  | OrElse (a, b) -> (match (a, b) with
                     | (True, True) -> true
                     | (True, False) -> true
                     | (False, True) -> true
                     | (False, False) -> false
                     | (a, b) -> eval (OrElse (bool_to_formula (eval a), bool_to_formula (eval b)))
                     )
  | Imply (a, b) -> (match (a, b) with
                    | (True, True) -> true
                    | (False, True) -> true
                    | (True, False) -> false
                    | (False, False) -> true
                    | (a, b) -> eval (Imply (bool_to_formula (eval a), bool_to_formula (eval b)))
                    )
  | Equal (c, d) -> eval_less (exp_to_int c, exp_to_int d)
