type formula = True
  |False
  |Not of formula
  |AndAlso of formula * formula
  |OrElse of formula * formula
  |Imply of formula * formula
  |Equal of exp * exp
and exp = Num of int
  |Plus of exp * exp
  |Minus of exp * exp

let rec plusminus x = match x with
  |Plus (a,b) -> (plusminus a) + (plusminus b)
  |Minus (a, b) -> (plusminus a)-(plusminus b)
  |Num (a) -> a


let rec eval_help f = match f with
  |False -> False
  |True -> True
  |Not value -> (match value with
      |True -> False
      |False -> True
      |_ -> eval_help(Not(eval_help value)))
  |AndAlso (value,value1)->
      (match value with
        |True ->
          (match value1 with
            |True -> True
            |False -> False
            |_ -> eval_help(AndAlso((eval_help value), (eval_help value1))))
        |False -> False
        |_ -> eval_help(AndAlso((eval_help value), (eval_help value1))))
  |OrElse (value, value1) ->
      (match value with
        |True -> True
        |False ->
          (match value1 with
            |True -> True
            |False -> False
            |_ -> eval_help(OrElse((eval_help value), (eval_help value1))))
        |_ -> eval_help(OrElse((eval_help value), (eval_help value1))))
  |Imply (value, value1) ->
      (match value with
        |False -> True
        |True ->
          (match value1 with
            |False -> False
            |True -> True
            |_ -> eval_help(Imply(eval_help(value), eval_help(value1))))
        |_ -> eval_help (Imply(eval_help(value), eval_help(value1))))
  |Equal (value, value1) ->
    (match value, value1 with
      |a, b -> if ((plusminus a)=(plusminus b)) then True else False )

let eval f = let returned = eval_help f in match returned with
  |True -> true
  |False -> false


