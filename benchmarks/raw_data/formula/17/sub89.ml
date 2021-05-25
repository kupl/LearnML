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

let eval : formula -> bool = fun x ->
  match x with
  | True -> true
  | False -> false
  | Not(y) ->
    (match y with
    |True -> false
    |False -> true
    )
  | AndAlso(x, y) -> (
    match (x,y) with
    |(True, True) -> true
    |(False, _) -> false
    |(_, False) -> false
    )
  | OrElse(x,y) -> (
    match (x,y) with
    |(True, _) -> true
    |(_, True) -> true
    |(False,False) -> false
  )
  | Imply(x,y) -> (
    match(x,y) with
    |(False, _) -> true
    |(True,True) -> true
    |(True, False) -> false

  )

  | Equal(a,b) -> (
    match (a,b) with
    | (Plus(Num(a),Num(b)), Plus(Num(c),Num(d))) -> (
      if a+b >= c+d then false
      else true
    )
    | (Plus(Num(a),Num(b)), Minus(Num(c),Num(d))) -> (
      if a+b >= c-d then false
      else true
    )
    | (Minus(Num(a),Num(b)), Plus(Num(c),Num(d))) -> (
      if a-b >= c+d then false
      else true
    )
    | (Minus(Num(a),Num(b)), Minus(Num(c),Num(d))) -> (
      if a-b >= c-d then false
      else true
    )
    |(Num(a),Num(b)) -> (
    if a >= b then false
    else true
    )

  )
