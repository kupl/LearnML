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

let rec eval fm =
  let rec calc exp =
    match exp with
    | Num n -> n
    | Plus (exp1, exp2) -> calc exp1 + calc exp2
    | Minus (exp1, exp2) -> calc exp1 - calc exp2 in
  
  match fm with
  | True -> true
  | False -> false
  | Not _fm ->
    if eval _fm = true then false else true
  | AndAlso (fm1, fm2) ->
    if eval fm1 = true && eval fm2 = true then true
    else false
  | OrElse (fm1, fm2) ->
    if eval fm1 = true || eval fm2 = true then true
    else false
  | Imply (fm1, fm2) ->
    if eval fm1 = true && eval fm2 = false then false
    else true
  | Equal (exp1, exp2) ->
    if calc exp1 = calc exp2 then true else false
