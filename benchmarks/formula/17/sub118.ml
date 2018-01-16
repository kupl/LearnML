type formula = TRUE
  |FALSE
  |NOT of formula
  |ANDALSO of formula * formula
  |ORELSE of formula * formula
  |IMPLY of formula * formula
  |LESS of expr * expr
and expr = NUM of int
  |PLUS of expr * expr
  |MINUS of expr * expr

let rec plusminus x = match x with
  |PLUS (a,b) -> (plusminus a) + (plusminus b)
  |MINUS (a, b) -> (plusminus a)-(plusminus b)
  |NUM (a) -> a


let rec eval_help f = match f with
  |FALSE -> FALSE
  |TRUE -> TRUE
  |NOT value -> (match value with
      |TRUE -> FALSE
      |FALSE -> TRUE
      |_ -> eval_help(NOT(eval_help value)))
  |ANDALSO (value,value1)->
      (match value with
        |TRUE ->
          (match value1 with
            |TRUE -> TRUE
            |FALSE -> FALSE
            |_ -> eval_help(ANDALSO((eval_help value), (eval_help value1))))
        |FALSE -> FALSE
        |_ -> eval_help(ANDALSO((eval_help value), (eval_help value1))))
  |ORELSE (value, value1) ->
      (match value with
        |TRUE -> TRUE
        |FALSE ->
          (match value1 with
            |TRUE -> TRUE
            |FALSE -> FALSE
            |_ -> eval_help(ORELSE((eval_help value), (eval_help value1))))
        |_ -> eval_help(ORELSE((eval_help value), (eval_help value1))))
  |IMPLY (value, value1) ->
      (match value with
        |FALSE -> TRUE
        |TRUE ->
          (match value1 with
            |FALSE -> FALSE
            |TRUE -> TRUE
            |_ -> eval_help(IMPLY(eval_help(value), eval_help(value1))))
        |_ -> eval_help (IMPLY(eval_help(value), eval_help(value1))))
  |LESS (value, value1) ->
    (match value, value1 with
      |a, b -> if ((plusminus a)<(plusminus b)) then TRUE else FALSE )

let eval f = let returned = eval_help f in match returned with
  |TRUE -> true
  |FALSE -> false


