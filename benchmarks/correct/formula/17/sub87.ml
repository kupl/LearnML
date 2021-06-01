type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula*formula
  | Equal of exp* exp
and exp=
  | Num of int
  | Plus of exp*exp
  | Minus of exp*exp

let rec exp_to_int (e: exp): int=
  match e with
  | Num i -> i
  | Plus (i1, i2) -> (exp_to_int i1)+ (exp_to_int i2)
  | Minus (i1, i2) -> (exp_to_int i1)- (exp_to_int i2)


let rec eval (fm: formula) : bool =
  match fm with
  | True -> true
  | False -> false
  | Not fm2 -> if (eval fm2) then false else true
  | AndAlso (fm1, fm2) ->
      if ((eval fm1) && (eval fm2) )
      then true
      else false
  | OrElse (fm1, fm2) ->
      if ((eval fm1) || (eval fm2) )
      then true
      else false
  | Imply (fm1, fm2) ->
      if (not (eval fm1) || (eval fm2))
      then true
      else false
  | Equal (exp1, exp2) ->
      if ((exp_to_int exp1) = (exp_to_int exp2))
      then true
      else false


